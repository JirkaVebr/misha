package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Statement.Rule
import com.mishaLang.ast.Language.Value
import com.mishaLang.error.ProgramError.{NonStringSelectorExpression, StackOverflow}
import com.mishaLang.error.{ProgramError, RuleHeadParseError}
import com.mishaLang.interpreter.RuleContext.RuleSelector
import com.mishaLang.interpreter.ops.StringOps
import com.mishaLang.interpreter.validators.SelectorValidator
import com.mishaLang.parser.ruleHead.SelectorParser
import org.parboiled2.ParseError

import scala.util.{Failure, Success, Try}

object RuleInterpreter {

	def run(rule: Rule)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		normalizeRuleHead(rule) match {
			case Failure(exception) => Failure(exception)
			case Success((rawRuleHead, stateAfterHead)) =>
				// TODO resolve the correct rule handling based on context

				val preProcessed = new SelectorPreprocessor(
					rawRuleHead, TermInterpreter.runParentSelector()(stateAfterHead)
				).preProcess()

				SelectorParser(preProcessed) match {
					case Failure(exception) => exception match {
						case parseError: ParseError => Failure(RuleHeadParseError(parseError, state.environment))
						case _ => Failure(exception)
					}
					case Success(rawSelector) =>
						SelectorValidator.validateSelector(rawSelector)(state) match {
							case Failure(exception) => Failure(exception)
							case Success(validSelector) =>
								stateAfterHead.environment.pushSubScope(RuleSelector(validSelector)) match {
									case Some(newEnvironment) =>
										StatementInterpreter.run(rule.body.content)(EnvironmentWithValue(newEnvironment, Value.Unit)) match {
											case fail: Failure[EnvWithValue] => fail
											case Success(result) =>
												Success(EnvironmentWithValue(result.environment.popSubScope().get, result.value))
										}
									case None =>
										state.fail(StackOverflow)
								}
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
								Success(strings ++ list.values.toList.asInstanceOf[List[Value.String]])
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
