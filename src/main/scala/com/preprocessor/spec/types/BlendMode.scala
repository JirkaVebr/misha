package com.preprocessor.spec.types

import com.preprocessor.ast.Language.Type.{Literal, Union}
import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.Symbol

object BlendMode extends Type {

	override def name: String = "BlendMode"

	override def apply(): Symbol.TypeSymbol#Value = Union(Set(
		Literal("color"),
		Literal("color-burn"),
		Literal("color-dodge"),
		Literal("darken"),
		Literal("difference"),
		Literal("exclusion"),
		Literal("hard-light"),
		Literal("hue"),
		Literal("lighten"),
		Literal("luminosity"),
		Literal("multiply"),
		Literal("normal"),
		Literal("overlay"),
		Literal("saturation"),
		Literal("screen"),
		Literal("soft-light")
	))

}
