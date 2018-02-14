package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.{BinaryOperation, Equals, Expression}
import com.preprocessor.ast.Ast.{Term, Type}
import com.preprocessor.ast.Ast.Value.Number
import com.preprocessor.ast.Symbol.ValueSymbol
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success, Try}

class BinaryOperationInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Binary operation interpreter"

	// TODO test subtyping checks

	it should "correctly perform assignment" in {
		val symbol = ValueSymbol("myVar")
		val variable = Term.Variable(symbol)
		val initialValue = Number(123)
		val targetValue = Number(456)

		assertThrows[ProgramError](run(BinaryOperation(Equals, variable, targetValue))(state))

		implicit val newState: EvalState =
			(state ~> (testEnvironment.updated(symbol)(ValueRecord(initialValue, Type.Number)), initialValue)).get

		assert(newState.environment.lookup(symbol).get.value == initialValue)

		val updatedState = run(BinaryOperation(Equals, variable, targetValue))(newState)
		assert(updatedState.value == targetValue)
		assert(updatedState.environment.lookup(symbol).get.value == targetValue)
	}


	protected def run(expression: BinaryOperation)(implicit state: EvalState): EvalState = {
		val result = BinaryOperationInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
