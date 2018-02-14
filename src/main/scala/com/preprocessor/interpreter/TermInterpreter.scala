package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.Expression
import com.preprocessor.ast.Ast.Term._
import com.preprocessor.ast.Ast.Value._
import com.preprocessor.ast.Ast.{Term, Value}
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success, Try}

object TermInterpreter {

	def run(term: Term)(implicit state: EvalState): Try[EvalState] = term match {
		case value: Value => value match {
			case Value.Unit => state ~> Value.Unit
			case primitiveValue: Primitive => state ~> primitiveValue
			case compositeValue: Composite => compositeValue match {
				case tuple: Tuple2Value => runTuple(tuple)
				case list: Value.List => runListValue(list)
			}
		}
		case symbol: MagicSymbol => symbol match {
			case ParentSelector => sys.error("todo") // TODO
		}
		case variable: Variable => runVariable(variable)
		case FunctionCall(function, arguments) => sys.error("todo") // TODO
		case list: Term.List => runListTerm(list)
		case MemberAccess(container, name) => sys.error("todo") // TODO
	}

	private def runTuple(tuple: Tuple2Value)(implicit state: EvalState): Try[EvalState] = {
		sys.error("todo") // TODO
	}

	private def runVariable(variable: Variable)(implicit state: EvalState): Try[EvalState] = {
		val variableValue = state.environment.lookup(variable.name)

		variableValue match {
			case Some(value) => Success(EvalState(state.environment, value.value, value.recordType))
			case None => state.fail(ProgramError.ReadingUndefinedVariable, variable)
		}
	}

	private def runListValue(list: Value.List)(implicit state: EvalState): Try[EvalState] = {
		sys.error("todo") // TODO find common supertype
	}

	private def runListTerm(list: Term.List)(implicit state: EvalState): Try[EvalState] = {
		val chainResult = Interpreter.chainRun[Expression](list.items.toList, state, ExpressionInterpreter.run(_)(_))

		chainResult match {
			case Failure(exception) => Failure(exception)
			case Success((items, finalState)) => runListValue(Value.List(items))(finalState)
		}
	}

}
