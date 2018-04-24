package com.mishaLang.interpreter

import com.mishaLang.ast.Language
import com.mishaLang.ast.Language.{Program, Value}
import com.mishaLang.ast.Language.Value.Value
import shapeless.HList.ListCompat._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class Interpreter(val program: Program) {

	val rootEnvironment: RootEnvironment = createRootEnvironment()

	def runProgram(): Try[EnvWithValue] =
		StatementInterpreter.run(program.program)(EnvironmentWithValue(rootEnvironment, Value.Unit))

	private def createRootEnvironment(): RootEnvironment =
		RootEnvironment()
}


object Interpreter {

	type EnvWithValues = AugmentedEnvironment[List[Value.Value]]
	type Evaluator[V <: Language.Node] = (V, EnvWithValue) => Try[EnvWithValue]


	/**
		* !!! The items are returned in *reversed* order !!!
		* For actual sequences this is more convenient anyway and for artificial sequences such as binary operations,
		* a polymorphic implementation is planned anyway.
		*/
	def chainRun[V <: Language.Node](items: scala.List[V], state: EnvWithValue, evaluate: Evaluator[V]): Try[EnvWithValues] = {
		@tailrec
		def run(items: scala.List[V], state: EnvWithValues, evaluate: Evaluator[V]): Try[EnvWithValues] =
			items match {
				case Nil => Success(state)
				case scala.collection.immutable.::(expression, tail) =>
					val evaluationResult = evaluate(expression, EnvironmentWithValue(state.environment))
					evaluationResult match {
						case Failure(reason) => Failure(reason)
						case Success(newValueEnvironment) =>
							run(tail, new EnvWithValues(
								newValueEnvironment.environment, newValueEnvironment.value :: state.value
							), evaluate)
					}
			}

		run(items, new EnvWithValues(state.environment, List.empty), evaluate)
	}

}
