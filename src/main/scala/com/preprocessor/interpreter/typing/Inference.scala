package com.preprocessor.interpreter.typing

import com.preprocessor.ast.Language.{Type, Value}

object Inference {

	def inferTypeForValue(value: Value.Value): Type.Any = value.valueType // TODO

}
