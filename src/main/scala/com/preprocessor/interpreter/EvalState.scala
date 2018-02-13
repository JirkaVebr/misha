package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.{Type, Value}

case class EvalState(environment: Environment, nodeType: Type.Any = Type.Any, value: Value.Value = Value.Unit) {

	@inline def ~>(typeValue: (Type.Any, Value.Value)): EvalState =
		EvalState(environment, typeValue._1, typeValue._2)
}

