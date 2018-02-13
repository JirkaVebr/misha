package com.preprocessor.error

import com.preprocessor.ast.Ast

class ProgramError(val errorCode: ProgramError.ProgramErrorCode, val node: Ast.Node) extends Error


object ProgramError {
	sealed trait ProgramErrorCode {
		def message(node: Ast.Node): String
	}

	def apply(errorCode: ProgramError.ProgramErrorCode, node: Ast.Node): ProgramError =
		new ProgramError(errorCode, node)

	case object UndefinedVariable extends ProgramErrorCode {
		override def message(node: Ast.Node): String = "Undefined variable"
	}
}
