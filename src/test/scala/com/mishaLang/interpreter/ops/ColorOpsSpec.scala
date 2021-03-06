package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.Rgba
import com.mishaLang.ast.NumberUnit.Percentage
import com.mishaLang.interpreter.BaseInterpreterSpec

class ColorOpsSpec extends BaseInterpreterSpec{

	behavior of "Color operations"

	it should "correctly add rgba colors" in {
		assert(ColorOps.addColors(Rgba(1, 2, 3, 4), Rgba(4, 3, 2, 1)) === Rgba(5, 5, 5, 5))
		assert(ColorOps.addColors(Rgba(255, 255, 255), Rgba(100, 100, 100)) === Rgba(255, 255, 255))
	}

	it should "correctly subtract rgba colors" in {
		assert(ColorOps.subtractColors(Rgba(100, 100, 100, 100), Rgba(200, 200, 200, 200)) === Rgba(0, 0, 0, 0))
		assert(ColorOps.subtractColors(Rgba(5, 4, 3, 2), Rgba(4, 3, 2, 1)) === Rgba(1, 1, 1, 1))
	}

	it should "correctly lighten/darken rgba colors" in {
		assert(ColorOps.darken(Rgba(255, 255, 255), Value.Number(10, Percentage)) === Rgba(230, 230, 230))
		assert(ColorOps.darken(Rgba(180, 212, 85), Value.Number(15, Percentage)) === Rgba(143, 176, 45))
		assert(ColorOps.darken(Rgba(100, 100, 100), Value.Number(110, Percentage)) === Rgba(0, 0, 0))
		assert(ColorOps.lighten(Rgba(240, 70, 21), Value.Number(25, Percentage)) === Rgba(248, 165, 141))
		assert(ColorOps.lighten(Rgba(100, 100, 100), Value.Number(110, Percentage)) === Rgba(255, 255, 255))
	}

	it should "correctly cast colors to strings" in {
		assert(ColorOps.toString(Rgba(255, 255, 255)) === Value.String("#fff"))
		assert(ColorOps.toString(Rgba(170, 187, 204)) === Value.String("#abc"))
		assert(ColorOps.toString(Rgba(123, 123, 123)) === Value.String("#7b7b7b"))
		assert(ColorOps.toString(Rgba(51, 51, 51, 16)) === Value.String("rgba(51, 51, 51, 0.062745)"))
	}

}
