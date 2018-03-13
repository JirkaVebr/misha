package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Statement.Rule
import com.preprocessor.ast.RuleContext.RuleSelector
import com.preprocessor.interpreter.ops.StringOps
import com.preprocessor.parser.selector.SelectorParser

import scala.util.{Failure, Success, Try}

object RuleInterpreter {

	def run(rule: Rule)(implicit state: EvalState): Try[EvalState] = {
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

}
