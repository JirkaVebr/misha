package com.preprocessor.interpreter.ops

import com.preprocessor.ast.Ast.Value.Rgba
import com.preprocessor.interpreter.BaseInterpreterSpec

class ColorOpsSpec extends BaseInterpreterSpec{

	behavior of "Color operations"

	it should "correctly add rgba colors" in {
		assert(ColorOps.addColors(Rgba(1, 2, 3, 4), Rgba(4, 3, 2, 1)) == Rgba(5, 5, 5, 5))
		assert(ColorOps.addColors(Rgba(255, 255, 255), Rgba(100, 100, 100)) == Rgba(255, 255, 255))
	}

	it should "correctly subtract rgba colors" in {
		assert(ColorOps.subtractColors(Rgba(100, 100, 100, 100), Rgba(200, 200, 200, 200)) == Rgba(0, 0, 0, 0))
		assert(ColorOps.subtractColors(Rgba(5, 4, 3, 2), Rgba(4, 3, 2, 1)) == Rgba(1, 1, 1, 1))
	}

}
