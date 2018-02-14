package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Program
import com.preprocessor.ast.{Ast, ValueRecord}

import scala.util.{Failure, Success, Try}

class Interpreter(val program: Program) {

	val rootEnvironment: RootEnvironment = createRootEnvironment()

	def runProgram(): Try[EvalState] =
		StatementInterpreter.run(program.program)(EvalState(rootEnvironment))

	private def createRootEnvironment(): RootEnvironment =
		new RootEnvironment // TODO, this may get more involved
}


object Interpreter {

	// TODO this isn't tail recursive, and so for longer lists this will be a total nightmare
	def chainRun[V <: Ast.Node](items: scala.List[V], state: EvalState, evaluate: (V, EvalState) => Try[EvalState]):
	Try[(scala.List[ValueRecord], EvalState)] =
		items match {
			case Nil => Success((scala.List[ValueRecord](), state))
			case expression :: tail =>
				val evaluationResult = evaluate(expression, state)
				evaluationResult match {
					case Failure(reason) => Failure(reason)
					case Success(newEvalState) =>
						val subsequent = chainRun(tail, newEvalState, evaluate)
						subsequent match {
							case Failure(exception) => Failure(exception)
							case Success((valueRecords, latestState)) =>
								Success((newEvalState.valueRecord :: valueRecords, latestState))
						}
				}
		}

}
