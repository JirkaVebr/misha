package com.preprocessor.interpreter

import com.preprocessor.BaseSpec
import com.preprocessor.ast.Language.Node

import scala.util.Try

class BaseInterpreterSpec extends BaseSpec {

	val testEnvironment: Environment = new Environment()

	implicit val state: EvalState = EvalState(testEnvironment)


	protected def run[N <: Node](interpret: (N) => Try[EvalState], input: N): EvalState =
		interpret(input).get

}
