package com.preprocessor.interpreter

import com.preprocessor.ast.Language
import com.preprocessor.ast.Language.{Program, Value}
import shapeless.HList.ListCompat._

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

	// TODO this isn't tail recursive, and so for longer lists this will be a total nightmare
	def chainRun[V <: Language.Node](items: scala.List[V], state: EnvWithValue, evaluate: (V, EnvWithValue) => Try[EnvWithValue]):
	Try[EnvWithValues] =
		items match {
			case Nil => Success(new EnvWithValues(state.environment, List.empty))
			case expression :: tail =>
				val evaluationResult = evaluate(expression, state)
				evaluationResult match {
					case Failure(reason) => Failure(reason)
					case Success(newValueEnvironment) =>
						val subsequent = chainRun(tail, newValueEnvironment, evaluate)
						subsequent match {
							case Failure(exception) => Failure(exception)
							case Success(latestState) =>
								latestState ~> (newValueEnvironment.value :: latestState.value)
						}
				}
		}

}
