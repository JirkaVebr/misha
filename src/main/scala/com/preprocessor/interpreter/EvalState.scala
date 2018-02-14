package com.preprocessor.interpreter

import com.preprocessor.ast.{Ast, ValueRecord}
import com.preprocessor.ast.Ast.{Type, Value}
import com.preprocessor.error.ProgramError.ProgramErrorCode
import com.preprocessor.error.{Error, ProgramError}

import scala.util.{Failure, Success}

case class EvalState(environment: Environment, valueRecord: ValueRecord = ValueRecord.empty) {

	@inline def ~>(value: Value.Value): Success[EvalState] =
		Success(EvalState(environment, ValueRecord(value, value.valueType)))

	@inline def ~>(newState: (Environment, Value.Value)): Success[EvalState] =
		Success(EvalState(newState._1, ValueRecord(newState._2, newState._2.valueType)))

	@inline def withNewType(newType: Type.Any): Success[EvalState] =
		Success(EvalState(environment, ValueRecord(valueRecord.value, newType)))

	@inline def withNewValue(newValue: Value.Value, newType: Type.Any): Success[EvalState] =
		Success(EvalState(environment, ValueRecord(newValue, newType)))

	def fail(errorCode: ProgramErrorCode, nodes: Ast.Node*): Failure[EvalState] =
		Failure(ProgramError(errorCode, this, (if (nodes.isEmpty) List(valueRecord.value) else nodes): _*))

	def failFatally(error: Error): Failure[EvalState] = Failure(error)
}

