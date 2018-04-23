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


	def chainRun[V <: Language.Node](items: scala.List[V], state: EnvWithValue, evaluate: (V, EnvWithValue) => Try[EnvWithValue]):
	Try[EnvWithValues] =
		_chainRun(items, new EnvWithValues(state.environment, List.empty), evaluate, state.value)

	@tailrec
	def _chainRun[V <: Language.Node](items: scala.List[V], state: EnvWithValues, evaluate: (V, EnvWithValue) => Try[EnvWithValue], originalValue: Value):
	Try[EnvWithValues] =
		items match {
			case Nil => Success(state)
			case expression :: tail =>
				val evaluationResult = evaluate(expression, new EnvWithValue(state.environment, originalValue))
				evaluationResult match {
					case Failure(reason) => Failure(reason)
					case Success(newValueEnvironment) => {
						// TODO The append (+:) operation has O(n) complexity. 
						_chainRun(tail, new EnvWithValues(newValueEnvironment.environment, state.value.:+(newValueEnvironment.value)), evaluate, originalValue)
					}
				}
		}

}
