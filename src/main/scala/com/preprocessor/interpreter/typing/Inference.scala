package com.preprocessor.interpreter.typing

import com.preprocessor.ast.Ast.{Type, Value}

object Inference {

	def inferTypeForValue(value: Value.Value): Type.Any = value.valueType // TODO

}
