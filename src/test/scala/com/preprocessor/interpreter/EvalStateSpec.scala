package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.{Type, Value}

class EvalStateSpec extends BaseInterpreterSpec {

	behavior of "EvalState"

	it should "correctly create new states" in {
		val environment = new Environment()
		val evalState = EvalState(environment)

		assert(evalState.environment == environment)
		assert(evalState.valueRecord.recordType == Type.Unit)
		assert(evalState.valueRecord.value == Value.Unit)

		val numberType = Type.Number
		val numberValue = Value.Number(123)
		val newState = evalState ~> numberValue

		assert(newState.get.environment == environment)
		assert(newState.get.valueRecord.recordType == numberType)
		assert(newState.get.valueRecord.value == numberValue)
	}
}
