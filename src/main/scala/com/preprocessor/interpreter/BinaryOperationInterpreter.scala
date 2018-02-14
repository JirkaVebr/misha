package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Term
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError.{AssigningToNonVariable, IllTypedAssignment, WritingUninitializedVariable}
import com.preprocessor.error.{CompilerError, ProgramError}

import scala.util.{Failure, Success, Try}

object BinaryOperationInterpreter {

	def run(binaryOperation: BinaryOperation)(implicit state: EvalState): Try[EvalState] = binaryOperation.operator match {
		case _: Assignment => runAssignment(binaryOperation.left, binaryOperation.right)
		case _ =>
			val chain = List(binaryOperation.left, binaryOperation.right)

			Interpreter.chainRun[Expression](chain, state, ExpressionInterpreter.run(_)(_)) match {
				case Failure(exception) => Failure(exception)
				case Success((left :: right :: Nil, newState)) => binaryOperation.operator match {
					case _: NumericOperator => sys.error("todo") // TODO
					case _: Comparison => sys.error("todo") // TODO
					case _: LogicalOperator => sys.error("todo") // TODO
					case _ => state.failFatally(CompilerError()) // TODO
				}
				case _ => state.failFatally(CompilerError()) // TODO
			}

		/*
			StringOperations.castToString(right) match {
				case Some(string) => newState ~> string
				case None => newState.fail(ProgramError.ConcatenatingIllegalOperand)
			}
		 */
	}

	private def runAssignment(left: Expression, right: Expression)(implicit state: EvalState): Try[EvalState] = left match {
		case Term.Variable(name) => ExpressionInterpreter.run(right) match {
			case Failure(exception) => Failure(exception)
			case Success(newState) =>
				if (newState.environment.isInScope(name)) {
					val valueRecord = newState.environment.lookup(name).get

					if (newState.valueRecord.recordType isSubtypeOf valueRecord.recordType)
						// TODO add readonly checks
						newState ~> (newState.environment.updated(name)(valueRecord ~> newState.valueRecord.value), newState.valueRecord.value)
					else newState.fail(IllTypedAssignment)
				}
				else newState.fail(WritingUninitializedVariable, left)
		}
		case _ => state.fail(AssigningToNonVariable, left)
	}

}
