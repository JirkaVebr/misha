package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Statement._
import com.preprocessor.ast.Language.{Statement, Value}
import com.preprocessor.ast.RuleContext.RuleSelector
import com.preprocessor.ast.Symbol.PropertySymbol
import com.preprocessor.ast.{PropertyRecord, ValueRecord}
import com.preprocessor.error.ProgramError._
import com.preprocessor.interpreter.ops.StringOps
import com.preprocessor.interpreter.typing.{Inference, Subtype}
import com.preprocessor.parser.selector.SelectorParser

import scala.util.{Failure, Success, Try}

object StatementInterpreter {

	def run(statement: Statement)(implicit state: EvalState): Try[EvalState] = statement match {
		case sequence: Sequence => runSequence(sequence)
		case typeAlias: TypeAliasDeclaration => runTypeAliasDeclaration(typeAlias)
		case variableDeclaration: VariableDeclaration => runVariableDeclaration(variableDeclaration)
		case rule: Rule => runRule(rule)
		case property: Statement.Property => runProperty(property)
		case NoOp => runNoOp()

		case expression: Expression => ExpressionInterpreter.run(expression)
	}

	private def runSequence(sequence: Sequence)(implicit state: EvalState): Try[EvalState] =
		Interpreter.chainRun[Statement](List(sequence.current, sequence.following), state, run(_)(_)) match {
			case Failure(exception) => Failure(exception)
			case Success((_, finalState)) => Success(finalState)
		}

	private def runTypeAliasDeclaration(typeAlias: TypeAliasDeclaration)(implicit state: EvalState): Try[EvalState] = {
		val existingType = state.environment.lookup(typeAlias.alias.name)

		if (existingType.nonEmpty && !Subtype.isSubtypeOf(typeAlias.subType, existingType.get)) {
			state.fail(NonNarrowingTypeAlias, typeAlias)
		} else {
			state.withNewSymbol(typeAlias.alias.name)(typeAlias.subType)
		}
	}

	private def runVariableDeclaration(varDeclaration: VariableDeclaration)(implicit state: EvalState): Try[EvalState] = {
		val declaration = varDeclaration.declaration
		if (state.environment.isInCurrentScope(declaration.name))
			state.fail(DuplicateVariableDeclaration, varDeclaration)
		else ExpressionInterpreter.run(declaration.value) match {
			case Failure(exception) => Failure(exception)
			case Success(stateAfterValue) => declaration.typeAnnotation match {
				case Some(annotatedType) =>
					if (Subtype.isSubtypeOf(stateAfterValue.valueRecord.recordType, annotatedType))
						stateAfterValue.withNewSymbol(declaration.name)(
							ValueRecord(stateAfterValue.valueRecord.value, annotatedType)
						)
					else
						stateAfterValue.fail(TypeAnnotationMismatch, varDeclaration)
				case None =>
					stateAfterValue.withNewSymbol(declaration.name)(
						ValueRecord(stateAfterValue.valueRecord.value, Inference.inferTypeForValue(stateAfterValue.valueRecord.value))
					)
			}
		}
	}

	private def runRule(rule: Rule)(implicit state: EvalState): Try[EvalState] = {
		val headExpressions = rule.head.foldRight(List.empty[Expression])({
			case (component, expressions) => component match {
				case Right(expression) => expression :: expressions
				case _ => expressions
			}
		})
		Interpreter.chainRun[Expression](
			headExpressions, state, ExpressionInterpreter.run(_)(_)
		) match {
			case Failure(exception) => Failure(exception)
			case Success((valueRecords, stateAfterHead)) =>
				val ruleHead: String = rule.head.foldRight((StringBuilder.newBuilder, valueRecords))({
					case (original, (components: StringBuilder, values)) => original match {
						case Left(string) => (components.append(string), values)
						case Right(_) => (components.append(StringOps.castToString(values.head.value)), values.tail)
					}
				})._1.toString
				val newScope = stateAfterHead.environment.pushSubScope(RuleSelector(SelectorParser(ruleHead).get)) // TODO

				StatementInterpreter.run(rule.body.content)(EvalState(newScope)) match {
					case fail: Failure[EvalState] => fail
					case Success(result) =>
						Success(EvalState(result.environment.popSubScope().get, result.valueRecord))
				}
		}
	}

	private def runProperty(property: Statement.Property)(implicit state: EvalState): Try[EvalState] =
		ExpressionInterpreter.run(property.name) match {
			case Failure(reason) => Failure(reason)
			case Success(stateAfterName) => stateAfterName.valueRecord.value match {
				case Value.String(name) => ExpressionInterpreter.run(property.value)(stateAfterName) match {
					case Failure(reason) => Failure(reason)
					case Success(stateAfterValue) => StringOps.castToString(stateAfterValue.valueRecord.value) match {
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

	private def runNoOp()(implicit state: EvalState): Try[EvalState] = Success(state)

}
