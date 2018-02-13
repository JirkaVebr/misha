package com.preprocessor.interpreter

import com.preprocessor.ast.Ast
import com.preprocessor.ast.Ast.{Type, Value}
import com.preprocessor.error.ProgramError
import com.preprocessor.error.ProgramError.ProgramErrorCode

import scala.util.{Failure, Success, Try}

case class EvalState(environment: Environment, nodeType: Type.Any = Type.Any, value: Value.Value = Value.Unit) {

	@inline def ~>(typeValue: (Type.Any, Value.Value)): Try[EvalState] =
		Success(EvalState(environment, typeValue._1, typeValue._2))

	@inline def ~>(value: Value.Value): Try[EvalState] = ~>(value.valueType, value)

	def fail(errorCode: ProgramErrorCode, node: Ast.Node = this.value): Failure[EvalState] =
		Failure(ProgramError(errorCode, node, this))
}

