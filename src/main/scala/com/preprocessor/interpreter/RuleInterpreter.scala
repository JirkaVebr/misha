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
		val isParentImplicit = RuleHeadPreprocessor.isParentImplicit(rule.head)

		normalizeRuleHead(rule) match {
			case Failure(exception) => Failure(exception)
			case Success((rawRuleHead, stateAfterHead)) =>
				val preProcessed = RuleHeadPreprocessor.explode(rawRuleHead)

				SelectorNormalizer.normalize(SelectorParser(preProcessed).get)(state.environment) match {
					case Failure(exception) => Failure(exception)
					case Success(normalized) =>
						val finalized = if (isParentImplicit) prependImplicitParent(normalized) else normalized

						// TODO validate finalized
						val newScope = stateAfterHead.environment.pushSubScope(RuleSelector(finalized))

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


	private def mapToStrings(hopefullyList: Value.Value)(implicit state: EnvWithValue) =
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


	private def prependImplicitParent(selector: NormalizedSelector)(implicit state: EnvWithValue): NormalizedSelector =
		state.environment.lookupContext() match {
			case Some(rule) => rule match {
				case RuleSelector(parentSelector) => RuleHeadPreprocessor.prependImplicitParent(parentSelector, selector)
				case _: AtRule => ???
			}
			case None => selector
		}

}
