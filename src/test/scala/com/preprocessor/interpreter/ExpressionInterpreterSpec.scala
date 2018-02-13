package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.{ArithmeticNegation, Expression, LogicalNegation, UnaryOperation}
import com.preprocessor.ast.Ast.Value
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success}

class ExpressionInterpreterSpec extends BaseInterpreterSpec {


	behavior of "Expression interpreter"

	it should "correctly negate booleans" in {
		assert(run(UnaryOperation(LogicalNegation, Value.Boolean(true))).value == Value.Boolean(false))
		assertThrows[ProgramError](run(UnaryOperation(LogicalNegation, Value.String("I shall not be negated"))))
	}

	it should "correctly negate numbers" in {
		assert(run(UnaryOperation(ArithmeticNegation, Value.Number(123))).value == Value.Number(-123))
		assertThrows[ProgramError](run(UnaryOperation(LogicalNegation, Value.String("I shall not be negated"))))
	}


	protected def run(expression: Expression): EvalState = {
		val result = ExpressionInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
