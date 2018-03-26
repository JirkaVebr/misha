package com.mishaLang.interpreter

import com.mishaLang.BaseSpec
import com.mishaLang.ast.Language.Node

import scala.util.Try

class BaseInterpreterSpec extends BaseSpec {

	val testEnvironment: Environment = RootEnvironment()

	implicit val state: EnvWithValue = EnvironmentWithValue(testEnvironment)


	protected def run[N <: Node](interpret: (N) => Try[EnvWithValue], input: N): EnvWithValue =
		interpret(input).get

}
