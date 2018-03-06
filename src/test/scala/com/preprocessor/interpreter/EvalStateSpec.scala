package com.preprocessor.interpreter

import com.preprocessor.ast.Language.{Type, Value}

class EvalStateSpec extends BaseInterpreterSpec {

	behavior of "EvalState"

	it should "correctly create new states" in {
		val environment = new Environment()
		val evalState = EvalState(environment)

		assert(evalState.environment === environment)
		assert(evalState.valueRecord.recordType === Type.Unit)
		assert(evalState.valueRecord.value === Value.Unit)

		val numberType = Type.Scalar
		val numberValue = Value.Scalar(123)
		val newState = evalState evaluatedTo numberValue

		assert(newState.get.environment === environment)
		assert(newState.get.valueRecord.recordType === numberType)
		assert(newState.get.valueRecord.value === numberValue)
	}
}
