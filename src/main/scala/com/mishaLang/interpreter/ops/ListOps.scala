package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value

object ListOps {


	// Properties

	def length(list: Value.List): Value.Scalar =
		Value.Scalar(list.values.length)

}
