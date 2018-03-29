package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Value

class AugmentedEnvironmentSpec extends BaseInterpreterSpec {

	behavior of "AugmentedEnvironment"

	it should "correctly create new states" in {
		val environment = new Environment()
		val augmented = AugmentedEnvironment[Value.Value](environment, Value.Unit)

		assert(augmented.environment === environment)
		assert(augmented.value === Value.Unit)

		val numberValue = Value.Number(123)
		val newState = augmented ~> numberValue

		assert(newState.get.environment === environment)
		assert(newState.get.value === numberValue)
	}
}
