package com.mishaLang.interpreter

import com.mishaLang.ast.Language
import com.mishaLang.ast.Language.Expression.Expression
import com.mishaLang.ast.Language.Value.Value
import com.mishaLang.ast.Language.{Program, Value}
import shapeless.HList._
import shapeless._

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


	type EnvWithStaticValues[+L <: HList] = AugmentedEnvironment[L]

	object productChainRun extends Poly2 {
		implicit def caseHNil[L <: HNil]: Case.Aux[L, Environment, Try[EnvWithStaticValues[L]]] =
			at[L, Environment](
				(hNil: L, env: Environment) => Success(AugmentedEnvironment(env, hNil))
			)

		implicit def caseHCons[H <: Expression, T <: HList, TT <: HList]
		(implicit tailCase: Case.Aux[T, Environment, Try[EnvWithStaticValues[TT]]]):
		Case.Aux[H :: T, Environment, Try[EnvWithStaticValues[Value :: TT]]] =
			at[H :: T, Environment] {
				case (expression :: tail, env) =>
					val evaluationResult = ExpressionInterpreter.run(expression)(EnvironmentWithValue(env))
					evaluationResult match {
						case Failure(reason) => Failure(reason)
						case Success(newValueEnvironment) =>
							val environment: Environment = newValueEnvironment.environment
							productChainRun.apply[T, Environment](tail, environment) match {
								case Failure(reason) => Failure(reason)
								case Success(latestState: EnvWithStaticValues[TT]) =>
									Success(AugmentedEnvironment(
										latestState.environment, newValueEnvironment.value :: latestState.value)
									)
							}
					}
			}
	}

}
