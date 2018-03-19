package com.preprocessor.interpreter

import com.preprocessor.BaseSpec
import com.preprocessor.ast.Language.Node

import scala.util.Try

class BaseInterpreterSpec extends BaseSpec {

	val testEnvironment: Environment = new Environment()

	implicit val state: EnvWithValue = EnvironmentWithValue(testEnvironment)


	protected def run[N <: Node](interpret: (N) => Try[EnvWithValue], input: N): EnvWithValue =
		interpret(input).get

}
