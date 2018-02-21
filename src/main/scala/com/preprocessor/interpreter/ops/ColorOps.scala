package com.preprocessor.interpreter.ops

import com.preprocessor.ast.Ast.Value.Rgba

object ColorOps {

	private def combineColors(x: Rgba, y: Rgba, combine: (Int, Int) => Int, normalize: (Int) => Int): Rgba =
		Rgba(
			normalize(combine(x.r, y.r)),
			normalize(combine(x.g, y.g)),
			normalize(combine(x.b, y.b)),
			normalize(combine(x.a, y.a))
		)


	def addColors(x: Rgba, y: Rgba): Rgba = combineColors(x, y, _ + _, _ min 255)

	def subtractColors(x: Rgba, y: Rgba): Rgba = combineColors(x, y, _ - _, _ max 0)

}
