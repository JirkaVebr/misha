package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Statement._
import com.preprocessor.ast.Language.{Statement, Value}
import com.preprocessor.ast.PropertyRecord
import com.preprocessor.error.ProgramError._
import com.preprocessor.interpreter.Symbol.PropertySymbol
import com.preprocessor.interpreter.ops.StringOps
import com.preprocessor.interpreter.typing.Subtype

import scala.util.{Failure, Success, Try}

object StatementInterpreter {

	def run(statement: Statement)(implicit state: EnvWithValue): Try[EnvWithValue] = statement match {
		case sequence: Sequence => runSequence(sequence)
		case typeAlias: TypeAliasDeclaration => runTypeAliasDeclaration(typeAlias)
		case variableDeclaration: VariableDeclaration => runVariableDeclaration(variableDeclaration)
		case rule: Rule => RuleInterpreter.run(rule)
		case property: Statement.Property => runProperty(property)
		case NoOp => runNoOp()

		case expression: Expression => ExpressionInterpreter.run(expression)
	}

	private def runSequence(sequence: Sequence)(implicit state: EnvWithValue): Try[EnvWithValue] =
		Interpreter.chainRun[Statement](List(sequence.current, sequence.following), state, run(_)(_)) match {
			case Failure(exception) => Failure(exception)
			case Success((_, finalState)) => Success(finalState)
		}

	private def runTypeAliasDeclaration(typeAlias: TypeAliasDeclaration)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val existingType = state.environment.lookup(typeAlias.alias.name)

		if (existingType.nonEmpty && !Subtype.isSubtypeOf(typeAlias.subType, existingType.get)) {
			state.fail(NonNarrowingTypeAlias, typeAlias)
		} else {
			state.withNewSymbol(typeAlias.alias.name)(typeAlias.subType)
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
					if (Subtype.isSubtypeOf(stateAfterValue.value.valueType, annotatedType))
						stateAfterValue.withNewSymbol(declaration.name)(stateAfterValue.value)
					else
						stateAfterValue.fail(TypeAnnotationMismatch, varDeclaration)
				case None =>
					stateAfterValue.withNewSymbol(declaration.name)(stateAfterValue.value) // TODO There used to be inference here
			}
		}
	}

	private def runProperty(property: Statement.Property)(implicit state: EnvWithValue): Try[EnvWithValue] =
		ExpressionInterpreter.run(property.name) match {
			case Failure(reason) => Failure(reason)
			case Success(stateAfterName) => stateAfterName.value match {
				case Value.String(name) => ExpressionInterpreter.run(property.value)(stateAfterName) match {
					case Failure(reason) => Failure(reason)
					case Success(stateAfterValue) => StringOps.castToString(stateAfterValue.value) match {
						case Some(valueString) =>
							val property = PropertyRecord(name, valueString.value, Set.empty) // TODO actually use flags

							stateAfterValue.withNewSymbol(PropertySymbol)(
								property :: (stateAfterValue.environment.lookupCurrent(PropertySymbol) match {
									case Some(properties) => properties
									case None => List.empty
								})
							)
						case None => stateAfterName.fail(IllegalPropertyValue, property.name)
					}
				}
				case _ => stateAfterName.fail(NonStringPropertyName, property.name)
			}
		}

	private def runNoOp()(implicit state: EnvWithValue): Try[EnvWithValue] = Success(state)

}
