package com.preprocessor.interpreter

import com.preprocessor.ast.Ast
import com.preprocessor.ast.Ast.{Type, Value}
import com.preprocessor.error.ProgramError.ProgramErrorCode
import com.preprocessor.error.{Error, ProgramError}

import scala.util.{Failure, Success}

case class EvalState(environment: Environment, value: Value.Value = Value.Unit, nodeType: Type.Any = Type.Any) {

	@inline def ~>(value: Value.Value): Success[EvalState] =
		Success(EvalState(environment, value, value.valueType))

	@inline def ~>(newState: (Environment, Value.Value)): Success[EvalState] =
		Success(EvalState(newState._1, newState._2, newState._2.valueType))

	def fail(errorCode: ProgramErrorCode, nodes: Ast.Node*): Failure[EvalState] =
		Failure(ProgramError(errorCode, this, (if (nodes.isEmpty) List(value) else nodes): _*))

	def failFatally(error: Error): Failure[EvalState] = Failure(error)
}

