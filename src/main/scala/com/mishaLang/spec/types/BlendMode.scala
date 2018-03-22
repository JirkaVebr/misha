package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type.{Literal, Union}
import com.mishaLang.ast.Language.Value
import com.mishaLang.interpreter.Symbol

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
