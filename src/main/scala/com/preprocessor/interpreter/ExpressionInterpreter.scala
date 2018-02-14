package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Term.Term
import com.preprocessor.ast.Ast.Value
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success, Try}

object ExpressionInterpreter {

	def run(expression: Expression)(implicit state: EvalState): Try[EvalState] = expression match {
		case binaryOperation: BinaryOperation => BinaryOperationInterpreter.run(binaryOperation)
		case unaryOperation: UnaryOperation => runUnaryOperation(unaryOperation)
		case Conditional(condition, consequent, alternative) => sys.error("todo") // TODO
		case StringInterpolation(components) => sys.error("todo") // TODO
		case term: Term => TermInterpreter.run(term)
	}

	private def runUnaryOperation(unaryOperation: UnaryOperation)(implicit state: EvalState): Try[EvalState] = {
		val operator = unaryOperation.operator
		val value = run(unaryOperation.value)

		value match {
			case Failure(_) => value
			case Success(newState) => operator match {
				case LogicalNegation => newState.value match {
					case Value.Boolean(bool) => newState ~> Value.Boolean(!bool)
					case _ => newState.fail(ProgramError.NegatingNonBoolean)
				}
				case ArithmeticNegation => newState.value match {
					case Value.Number(magnitude, unit) => newState ~> Value.Number(-magnitude, unit)
					case _ => newState.fail(ProgramError.NegatingNonBoolean)
				}
			}
		}
	}
}
