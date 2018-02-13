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
		val numbervalue = Value.Number(123)
		val newState = evalState ~> (numberType, numbervalue)

		assert(newState.environment == environment)
		assert(newState.nodeType == numberType)
		assert(newState.value == numbervalue)
	}
}
