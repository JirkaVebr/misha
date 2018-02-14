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

	case object UndefinedVariable extends SimpleError("Undefined variable")

	case object NegatingNonBoolean extends SimpleError("Logical negation of a non-boolean")
	case object NegatingNonNumeric extends SimpleError("Arithmetic negation of a non-numeric")
}
