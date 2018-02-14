package com.preprocessor.error

import com.preprocessor.ast.Ast
import com.preprocessor.interpreter.EvalState

class ProgramError(val errorCode: ProgramError.ProgramErrorCode, val evalState: EvalState, val nodes: Ast.Node*) extends Error


object ProgramError {
	sealed trait ProgramErrorCode {
		def message(nodes: Ast.Node*): String
	}

	abstract class SimpleError(val message: String) extends ProgramErrorCode {
		override def message(nodes: Ast.Node*): String = message
	}

	def apply(errorCode: ProgramError.ProgramErrorCode, evalState: EvalState, nodes: Ast.Node*): ProgramError =
		new ProgramError(errorCode, evalState, nodes: _*)

	case object AssigningToNonVariable extends SimpleError("Assignment to a non-variable")
	case object ConcatenatingIllegalOperand extends SimpleError("Concatenation of a value that is not convertible to a string")
	case object IllTypedAssignment extends SimpleError("Type of assignment value does not conform to the declared variable type")
	case object IllTypedConditionBranches extends SimpleError("Miss-matched types of branches of a conditional expression")
	case object NegatingNonBoolean extends SimpleError("Logical negation of a non-boolean")
	case object NegatingNonNumeric extends SimpleError("Arithmetic negation of a non-numeric")
	case object NonBooleanCondition extends SimpleError("Non-boolean condition value")
	case object ReadingUndefinedVariable extends SimpleError("Reading of an undefined variable")
	case object WritingUninitializedVariable extends SimpleError("Writing to an uninitialized variable")
}
