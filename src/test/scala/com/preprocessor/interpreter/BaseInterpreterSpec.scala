package com.preprocessor.interpreter

import com.preprocessor.BaseSpec

class BaseInterpreterSpec extends BaseSpec {

	val testEnvironment: Environment = new Environment()

	implicit val state: EvalState = EvalState(testEnvironment)

}
