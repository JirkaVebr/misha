package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Statement.Rule
import com.preprocessor.ast.Language.Term.ParentSelector
import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.Selector.NormalizedSelector
import com.preprocessor.error.ProgramError
import com.preprocessor.error.ProgramError.NonStringSelectorExpression
import com.preprocessor.interpreter.RuleContext.{AtRule, RuleSelector}
import com.preprocessor.interpreter.ops.StringOps
import com.preprocessor.interpreter.validators.SelectorNormalizer
import com.preprocessor.parser.ruleHead.SelectorParser

import scala.util.{Failure, Success, Try}

object RuleInterpreter {

	def run(rule: Rule)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		normalizeRuleHead(rule) match {
			case Failure(exception) => Failure(exception)
			case Success((rawRuleHead, stateAfterHead)) =>
				// TODO resolve the correct rule handling based on context

				val preProcessed = new SelectorPreprocessor(rawRuleHead).preProcess()

				SelectorNormalizer.normalize(SelectorParser(preProcessed).get) match {
					case Failure(exception) => Failure(exception)
					case Success(normalized) =>
						// TODO validate normalized
						val newScope = stateAfterHead.environment.pushSubScope(RuleSelector(normalized))

						StatementInterpreter.run(rule.body.content)(EnvironmentWithValue(newScope, Value.Unit)) match {
							case fail: Failure[EnvWithValue] => fail
							case Success(result) =>
								Success(EnvironmentWithValue(result.environment.popSubScope().get, result.value))
						}
				}
		}
	}


	private def normalizeRuleHead(rule: Rule)(implicit state: EnvWithValue) =
		rule.head.foldLeft[Try[(RawRuleHead, EnvWithValue)]](
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
					case Right(expression) =>
						ExpressionInterpreter.run(expression)(currentState) match {
							case Failure(exception) => Failure(exception)
							case Success(newEnvironment) =>
								mapToStrings(newEnvironment.value) match {
									case Failure(exception) => Failure(exception)
									case Success(strings) => Success(
										(if (strings.isEmpty) rest else rest :+ Right(strings), EnvironmentWithValue(newEnvironment.environment))
									)
								}
						}
				}
			}
		}


	private def mapToStrings(hopefullyList: Value.Value)(implicit state: EnvWithValue): Try[Vector[Value.String]] =
		hopefullyList match {
			case list: Value.List => list.values.foldLeft[Try[Vector[Value.String]]](Success(Vector.empty[Value.String])) {
				case (accumulator, value) => accumulator match {
					case Failure(_) => accumulator
					case Success(strings) => value match {
						case list: Value.List =>
							if (list.values.forall(_.isInstanceOf[Value.String]))
								Success(strings ++ list.values.asInstanceOf[List[Value.String]])
							else
								Failure(ProgramError(NonStringSelectorExpression, state, value))
						case _ => StringOps.castToString(value) match {
							case Some(string) => Success(strings :+ string)
							case None => Failure(ProgramError(NonStringSelectorExpression, state, value))
						}
					}
				}
			}
			case _ => Failure(ProgramError(NonStringSelectorExpression, state, hopefullyList))
		}

}
