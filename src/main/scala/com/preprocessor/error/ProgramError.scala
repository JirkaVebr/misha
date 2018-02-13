package com.preprocessor.error

import com.preprocessor.ast.Ast
import com.preprocessor.interpreter.EvalState

class ProgramError(val errorCode: ProgramError.ProgramErrorCode, val node: Ast.Node, val evalState: EvalState) extends Error


object ProgramError {
	sealed trait ProgramErrorCode {
		def message(node: Ast.Node): String
	}

	abstract class SimpleError(val message: String) extends ProgramErrorCode {
		override def message(node: Ast.Node): String = message
	}

	def apply(errorCode: ProgramError.ProgramErrorCode, node: Ast.Node, evalState: EvalState): ProgramError =
		new ProgramError(errorCode, node, evalState)

	case object UndefinedVariable extends SimpleError("Undefined variable")

	case object NegatingNonBoolean extends SimpleError("Logical negation of a non-boolean")
	case object NegatingNonNumeric extends SimpleError("Arithmetic negation of a non-numeric")
}
