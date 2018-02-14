package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.{Type, Value}

class EvalStateSpec extends BaseInterpreterSpec {

	behavior of "EvalState"

	it should "correctly create new states" in {
		val environment = new Environment()
		val evalState = EvalState(environment)

		assert(evalState.environment == environment)
		assert(evalState.nodeType == Type.Any)
		assert(evalState.value == Value.Unit)

		val numberType = Type.Number
		val numberValue = Value.Number(123)
		val newState = evalState ~> numberValue

		assert(newState.get.environment == environment)
		assert(newState.get.nodeType == numberType)
		assert(newState.get.value == numberValue)
	}
}
