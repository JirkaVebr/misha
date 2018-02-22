package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Value
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success}

class ExpressionInterpreterSpec extends BaseInterpreterSpec {


	behavior of "Expression interpreter"

	it should "correctly negate booleans" in {
		assert(run(UnaryOperation(LogicalNegation, Value.Boolean(true))).valueRecord.value == Value.Boolean(false))
		assertThrows[ProgramError](run(UnaryOperation(LogicalNegation, Value.String("I shall not be negated"))))
	}

	it should "correctly negate numbers" in {
		assert(run(UnaryOperation(ArithmeticNegation, Value.Scalar(123))).valueRecord.value == Value.Scalar(-123))
		assertThrows[ProgramError](run(UnaryOperation(LogicalNegation, Value.String("I shall not be negated"))))
	}

	it should "correctly execute conditional expressions" in {
		val consequent = Value.Scalar(123)
		val alternative = Value.Scalar(456)

		assert(run(Conditional(Value.Boolean(true), consequent, None)).valueRecord.value == consequent)
		assert(run(Conditional(Value.Boolean(true), consequent, Some(alternative))).valueRecord.value == consequent)
		assert(run(Conditional(Value.Boolean(false), consequent, Some(alternative))).valueRecord.value == alternative)
		assertThrows[ProgramError](run(Conditional(consequent, consequent, None)))
		assertThrows[ProgramError](run(Conditional(Value.Boolean(true), consequent, Some(Value.Boolean(true)))))
	}


	protected def run(expression: Expression): EvalState = {
		val result = ExpressionInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
