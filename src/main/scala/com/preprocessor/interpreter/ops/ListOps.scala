package com.preprocessor.interpreter.ops

import com.preprocessor.ast.Language.Value

object ListOps {


	// Properties

	def length(list: Value.List): Value.Scalar =
		Value.Scalar(list.values.length)

}
