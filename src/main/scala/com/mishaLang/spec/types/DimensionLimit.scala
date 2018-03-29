package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.TypeAlias
import com.mishaLang.interpreter.Symbol.TypeSymbol

/**
	* The idea of this is to have only one type for {min-max}{width-height}
	*/
object DimensionLimit extends Type {


	override def name: TypeSymbol = "DimensionLimit"

	override def apply(): Type.Any = Type.Union(Set(
		Type.Length,
		Type.Percentage,
		Type.Literal("none"),
		Type.Literal("max-content"),
		Type.Literal("min-content"),
		Type.Literal("fit-content"),
		Type.Literal("fill-available"),
		TypeAlias(Global.name)
	))
}
