package com.preprocessor.spec.types

import com.preprocessor.ast.Language.Type.{Literal, Union}
import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.Symbol

object BlendMode extends Type {

	override def name: String = "BlendMode"

	override def apply(): Symbol.TypeSymbol#Value = Union(Set(
		Literal(Value.String("color")),
		Literal(Value.String("color-burn")),
		Literal(Value.String("color-dodge")),
		Literal(Value.String("darken")),
		Literal(Value.String("difference")),
		Literal(Value.String("exclusion")),
		Literal(Value.String("hard-light")),
		Literal(Value.String("hue")),
		Literal(Value.String("lighten")),
		Literal(Value.String("luminosity")),
		Literal(Value.String("multiply")),
		Literal(Value.String("normal")),
		Literal(Value.String("overlay")),
		Literal(Value.String("saturation")),
		Literal(Value.String("screen")),
		Literal(Value.String("soft-light"))
	))

}
