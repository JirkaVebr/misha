package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Term._
import com.preprocessor.ast.Language.Value._
import com.preprocessor.ast.Language.{Term, Value}
import RuleContext.{AtRule, RuleSelector}
import com.preprocessor.emitter.RuleHeadEmitter
import com.preprocessor.error.CompilerError
import com.preprocessor.error.ProgramError.ReadingUndefinedVariable

import scala.util.{Failure, Success, Try}

object TermInterpreter {

	def run(term: Term)(implicit state: EvalState): Try[EvalState] = term match {
		case value: Value => value match {
			case Value.Unit => state evaluatedTo Value.Unit
			case primitiveValue: Primitive => state evaluatedTo primitiveValue
			case compositeValue: Composite => compositeValue match {
				case tuple: Value.Tuple2 => runTupleValue(tuple)
				case list: Value.List => runListValue(list)
			}
		}
		case symbol: MagicSymbol => runMagicSymbol(symbol)
		case variable: Variable => runVariable(variable)
		case FunctionCall(function, arguments) => sys.error("todo") // TODO
		case tuple: Term.Tuple2 => runTupleTerm(tuple)
		case list: Term.List => runListTerm(list)
		case MemberAccess(container, name) => sys.error("todo") // TODO
	}

	private def runTupleValue(tuple: Value.Tuple2)(implicit state: EvalState): Try[EvalState] =
		state evaluatedTo tuple

	private def runTupleTerm(tuple: Term.Tuple2)(implicit state: EvalState): Try[EvalState] = {
		Interpreter.chainRun[Expression](scala.List(tuple.first, tuple.second), state, ExpressionInterpreter.run(_)(_)) match {
			case Failure(reason) => Failure(reason)
			case Success((first :: second :: Nil, finalState)) =>
				finalState ~> Value.Tuple2(first.value, second.value)
			case _ => state.failFatally(CompilerError("TODO")) // TODO
		}
	}

	private def runMagicSymbol(symbol: MagicSymbol)(implicit state: EvalState): Try[EvalState] = symbol match {
		case ParentSelector => state.environment.lookupContext() match {
			case Some(context) => state.evaluatedTo(Value.String(
				RuleHeadEmitter.emit(context)(StringBuilder.newBuilder).mkString
			))
			case None => state.evaluatedTo(Value.String("")) // throw error?
		}
	}

	private def runVariable(variable: Variable)(implicit state: EvalState): Try[EvalState] = {
		val variableValue = state.environment.lookup(variable.name)

		variableValue match {
			case Some(value) => state ~> value.value
			case None => state.fail(ReadingUndefinedVariable, variable)
		}
	}

	private def runListValue(list: Value.List)(implicit state: EvalState): Try[EvalState] = {
		sys.error("todo") // TODO find common supertype
	}

	private def runListTerm(list: Term.List)(implicit state: EvalState): Try[EvalState] = {
		val chainResult = Interpreter.chainRun[Expression](list.items.toList, state, ExpressionInterpreter.run(_)(_))

		sys.error("todo")
		/*chainResult match {
			case Failure(exception) => Failure(exception)
			case Success((items, finalState)) => runListValue(Value.List(items))(finalState)
		}*/
	}

}
