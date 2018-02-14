package com.preprocessor.interpreter

import com.preprocessor.ast.Ast
import com.preprocessor.ast.Ast.{Type, Value}
import com.preprocessor.error.{Error, ProgramError}
import com.preprocessor.error.ProgramError.ProgramErrorCode

import scala.util.{Failure, Success, Try}

case class EvalState(environment: Environment, nodeType: Type.Any = Type.Any, value: Value.Value = Value.Unit) {

	@inline def ~>(value: Value.Value): Try[EvalState] =
		Success(EvalState(environment, value.valueType, value))

	def fail(errorCode: ProgramErrorCode, nodes: Ast.Node*): Failure[EvalState] =
		Failure(ProgramError(errorCode, this, (if (nodes.isEmpty) List(value) else nodes): _*))

	def failFatally(error: Error): Failure[EvalState] = Failure(error)
}

