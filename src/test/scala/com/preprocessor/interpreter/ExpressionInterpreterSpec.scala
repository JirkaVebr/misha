package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.{Expression, LogicalNegation, UnaryOperation}
import com.preprocessor.ast.Ast.Value

import scala.util.{Failure, Success}

class ExpressionInterpreterSpec extends BaseInterpreterSpec {


	behavior of "Expression interpreter"

	it should "correctly negate booleans" in {
		assert(run(UnaryOperation(LogicalNegation, Value.Boolean(true))).value == Value.Boolean(false))
	}


	protected def run(expression: Expression): EvalState = {
		val result = ExpressionInterpreter.run(expression)

		result match {
			case Failure(exception) => fail(exception)
			case Success(value) => value
		}
	}
}
