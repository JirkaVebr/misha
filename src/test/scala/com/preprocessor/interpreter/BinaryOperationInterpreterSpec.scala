package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Value.Number
import com.preprocessor.ast.Ast.{Term, Type, Value}
import com.preprocessor.ast.Symbol.ValueSymbol
import com.preprocessor.ast.UnitOfMeasure.Percentage
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success}

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
			state.withUpdatedValue(symbol, ValueRecord(initialValue, Type.Number)).get

		assert(newState.environment.lookup(symbol).get.value == initialValue)

		val updatedState = run(BinaryOperation(Equals, variable, targetValue))(newState)
		assert(updatedState.valueRecord.value == targetValue)
		assert(updatedState.environment.lookup(symbol).get.value == targetValue)
	}

	it should "correctly perform comparison" in {
		assert(run(BinaryOperation(IsEqualTo, Number(123), Number(123))).valueRecord.value == Value.Boolean(true))
		assert(run(BinaryOperation(IsEqualTo, Number(123), Number(456))).valueRecord.value == Value.Boolean(false))
		assert(run(BinaryOperation(IsEqualTo, Number(123), Value.Boolean(true))).valueRecord.value == Value.Boolean(false))
		assert(run(BinaryOperation(LowerThan, Number(123), Number(456))).valueRecord.value == Value.Boolean(true))
		assert(run(BinaryOperation(LowerEquals, Number(123), Number(456))).valueRecord.value == Value.Boolean(true))
		assert(run(BinaryOperation(GreaterEquals, Number(456), Number(456))).valueRecord.value == Value.Boolean(true))
	}

	it should "reject illegal comparisons" in {
		assertThrows[ProgramError](run(BinaryOperation(GreaterEquals, Number(456), Value.Boolean(true))))
		assertThrows[ProgramError](run(BinaryOperation(GreaterEquals, Number(456), Number(456, Percentage))))
	}


	protected def run(expression: BinaryOperation)(implicit state: EvalState): EvalState = {
		val result = BinaryOperationInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
