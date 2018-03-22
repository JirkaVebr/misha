package com.mishaLang.interpreter.typing

import com.mishaLang.ast.Language.{Type, Value}

object Inference {

	def inferTypeForValue(value: Value.Value): Type.Any = value.valueType // TODO

}
