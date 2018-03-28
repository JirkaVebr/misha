package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression.{Block, Expression}
import com.mishaLang.ast.Language.Statement._
import com.mishaLang.ast.Language.{Statement, Type, Value, ValueSymbolDeclaration}
import com.mishaLang.ast.PropertyRecord
import com.mishaLang.error.ProgramError._
import com.mishaLang.interpreter.Symbol.RuleStoreSymbol
import com.mishaLang.interpreter.ops.StringOps
import com.mishaLang.interpreter.typing.{Subtype, Typing}
import com.mishaLang.utils.LinkedMap

import scala.util.{Failure, Success, Try}

object StatementInterpreter {

	def run(statement: Statement)(implicit state: EnvWithValue): Try[EnvWithValue] = statement match {
		case sequence: Sequence => runSequence(sequence)
		case typeAlias: TypeAliasDeclaration => runTypeAliasDeclaration(typeAlias)
		case variableDeclaration: VariableDeclaration => runVariableDeclaration(variableDeclaration)
		case each: Each => runEach(each)
		case rule: Rule => RuleInterpreter.run(rule)
		case property: Statement.Property => runProperty(property)
		case NoOp => runNoOp()

		case expression: Expression => ExpressionInterpreter.run(expression)
	}

	private def runSequence(sequence: Sequence)(implicit state: EnvWithValue): Try[EnvWithValue] =
		Interpreter.chainRun[Statement](List(sequence.current, sequence.following), state, run(_)(_)) match {
			case Failure(exception) => Failure(exception)
			case Success(newEnvironment) =>
				val (_ :: secondValue :: Nil) = newEnvironment.value
				Success(EnvironmentWithValue(newEnvironment.environment, secondValue))
		}

	private def runTypeAliasDeclaration(typeAlias: TypeAliasDeclaration)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val existingType = state.environment.lookup(typeAlias.alias.name)

		if (existingType.nonEmpty && !Subtype.isSubtypeOf(typeAlias.subType, existingType.get)) {
			state.fail(NonNarrowingTypeAlias, typeAlias)
		} else {
			state.withNewSymbol(typeAlias.alias.name)(typeAlias.subType)
		}
	}


	private def runEach(each: Each)(implicit state: EnvWithValue): Try[EnvWithValue] =
		ExpressionInterpreter.run(each.iterable) match {
			case Failure(exception) => Failure(exception)
			case Success(stateAfterIterable) =>
				stateAfterIterable.value match {
					case iterable: Value.List =>
						if (iterable.values.nonEmpty) {
							val steps = iterable.values.map {
								value =>
									Block(
										Sequence(
											VariableDeclaration(ValueSymbolDeclaration(each.iterator.name, None, value)),
											each.body.content
										)
									)
							}
							Interpreter.chainRun[Block](steps.toList, stateAfterIterable, ExpressionInterpreter.runBlock(_)(_)) match {
								case Failure(exception) => Failure(exception)
								case Success(stateAfterIterations) =>
									Success(EnvironmentWithValue(stateAfterIterations.environment, stateAfterIterations.value.last))
							}
						} else
							Success(stateAfterIterable)
					case _ => state.fail(NonListIterable, each)
				}
		}


		private def runVariableDeclaration(varDeclaration: VariableDeclaration)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val declaration = varDeclaration.declaration
		if (state.environment.isInCurrentScope(declaration.name))
			state.fail(DuplicateVariableDeclaration, varDeclaration)
		else ExpressionInterpreter.run(declaration.value) match {
			case Failure(exception) => Failure(exception)
			case Success(stateAfterValue) => declaration.typeAnnotation match {
				case Some(annotatedType) =>
					if (Typing.canBeAssignedTo(stateAfterValue.value, annotatedType))
						stateAfterValue.withNewSymbol(declaration.name)(stateAfterValue.value)
					else
						stateAfterValue.fail(TypeAnnotationMismatch, varDeclaration)
				case None =>
					stateAfterValue.withNewSymbol(declaration.name)(stateAfterValue.value)
			}
		}
	}

	private def runProperty(property: Statement.Property)(implicit state: EnvWithValue): Try[EnvWithValue] =
		state.environment.lookupContext() match {
			case Some(ruleContext) =>
				ExpressionInterpreter.run(property.name) match {
					case Failure(reason) => Failure(reason)
					case Success(stateAfterName) => stateAfterName.value match {
						case Value.String(name) => ExpressionInterpreter.run(property.value)(stateAfterName) match {
							case Failure(reason) => Failure(reason)
							case Success(stateAfterValue) => StringOps.castToString(stateAfterValue.value) match {
								case Some(valueString) =>
									// .get because we're assuming we're inside the RootEnvironment, which, you know, should really better hold
									val ruleStore: RuleStore = stateAfterValue.environment.lookup(RuleStoreSymbol).get
									val propertyStore: PropertyStore = ruleStore.getOrElse(ruleContext, LinkedMap.empty)
									val propertyRecord = PropertyRecord(name, stateAfterValue.value, valueString.value, Set.empty) // TODO actually use flags

									val newPropertyRecords: List[PropertyRecord] = propertyStore.get(name) match {
										case Some(propertyRecords) =>
											propertyRecord :: propertyRecords
										case None =>
											propertyRecord :: Nil
									}

									stateAfterValue.withUpdatedSymbol(RuleStoreSymbol)(
										ruleStore.updated(ruleContext, propertyStore.updated(name, newPropertyRecords))
											.asInstanceOf[RuleStoreSymbol.Value]
									)
								case None => stateAfterName.fail(IllegalPropertyValue, property.name)
							}
						}
						case _ => stateAfterName.fail(NonStringPropertyName, property.name)
					}
				}
			case None =>
				state.fail(PropertyOutsideARule, property)
		}

	private def runNoOp()(implicit state: EnvWithValue): Try[EnvWithValue] = Success(state)

}
