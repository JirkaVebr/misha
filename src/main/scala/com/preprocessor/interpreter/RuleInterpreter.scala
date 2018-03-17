package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Statement.Rule
import com.preprocessor.ast.Language.Term.ParentSelector
import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError
import com.preprocessor.error.ProgramError.NonStringSelectorExpression
import com.preprocessor.interpreter.RuleContext.RuleSelector
import com.preprocessor.interpreter.ops.StringOps
import com.preprocessor.interpreter.validators.SelectorNormalizer
import com.preprocessor.parser.ruleHead.SelectorParser

import scala.util.{Failure, Success, Try}

object RuleInterpreter {

	def run(rule: Rule)(implicit state: EvalState): Try[EvalState] = {

		normalizeRuleHead(rule) match {
			case Failure(exception) => Failure(exception)
			case Success((rawRuleHead, stateAfterHead)) =>
				val preProcessed = RuleHeadPreprocessor.explode(rawRuleHead)
				val newScope = stateAfterHead.environment.pushSubScope(RuleSelector(
					SelectorNormalizer.normalize(SelectorParser(preProcessed).get)(state.environment).get
				)) // TODO

				StatementInterpreter.run(rule.body.content)(EvalState(newScope)) match {
					case fail: Failure[EvalState] => fail
					case Success(result) =>
						Success(EvalState(result.environment.popSubScope().get, result.valueRecord))
				}
		}
	}


	private def normalizeRuleHead(rule: Rule)(implicit state: EvalState) = {
		val withImplicitParent = RuleHeadPreprocessor.prependImplicitParent(rule.head)

		withImplicitParent.foldLeft[Try[(RawRuleHead, EvalState)]](
			Success((Vector.empty[RawRuleHeadComponent], state))) {
			case (accumulator, ruleHeadComponent) => accumulator match {
				case Failure(_) => accumulator
				case Success((rest, currentState)) => ruleHeadComponent match {
					case Left(string) => rest.lastOption match {
						case Some(last) => last match {
							// Don't want several strings in a row
							case Left(lastString) => Success(rest.dropRight(1) :+ Left(lastString + string), currentState)
							case Right(_) => Success(rest :+ Left(string), currentState)
						}
						case None => Success(rest :+ Left(string), currentState)
					}
					case Right(expressions) =>
						Interpreter.chainRun[Expression](
							expressions.toList, currentState, ExpressionInterpreter.run(_)(_)
						) match {
							case Failure(exception) => Failure(exception)
							case Success((valueRecords, newState)) =>
								mapToStrings(valueRecords) match {
									case Failure(exception) => Failure(exception)
									case Success(strings) => Success(
										(rest :+ Right(strings), newState)
									)
								}
						}
				}
			}
		}
	}


	private def mapToStrings(valueRecords: List[ValueRecord])(implicit state: EvalState) =
		valueRecords.foldLeft[Try[Vector[Value.String]]](Success(Vector.empty[Value.String])) {
			case (accumulator, valueRecord) => accumulator match {
				case Failure(_) => accumulator
				case Success(strings) => StringOps.castToString(valueRecord.value) match {
					case Some(string) => Success(strings :+ string)
					case None => Failure(ProgramError(NonStringSelectorExpression, state, valueRecord.value))
				}
			}
		}

}
