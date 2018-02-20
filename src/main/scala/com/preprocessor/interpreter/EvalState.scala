package com.preprocessor.interpreter

import com.preprocessor.ast.{Ast, ValueRecord}
import com.preprocessor.ast.Ast.{Type, Value}
import com.preprocessor.ast.Symbol.Symbol
import com.preprocessor.error.ProgramError.ProgramErrorCode
import com.preprocessor.error.{Error, ProgramError}

import scala.util.{Failure, Success}

case class EvalState(environment: Environment, valueRecord: ValueRecord = ValueRecord.empty) {

	@inline def ~>(value: Value.Value): Success[EvalState] =
		Success(EvalState(environment, valueRecord ~> value))

	@inline def ~>(newType: Type.Any): Success[EvalState] =
		Success(EvalState(environment, valueRecord ~> newType))

	@inline def evaluatedTo(value: Value.Value): Success[EvalState] =
	Success(EvalState(environment, ValueRecord(value, value.valueType)))

	@inline def withUpdatedSymbol(symbol: Symbol)(value: symbol.Value): Success[EvalState] =
		Success(EvalState(environment.updated(symbol)(value), valueRecord))

	def fail(errorCode: ProgramErrorCode, nodes: Ast.Node*): Failure[EvalState] =
		Failure(ProgramError(errorCode, this, (if (nodes.isEmpty) List(valueRecord.value) else nodes): _*))

	def failFatally(error: Error): Failure[EvalState] = Failure(error)
}

