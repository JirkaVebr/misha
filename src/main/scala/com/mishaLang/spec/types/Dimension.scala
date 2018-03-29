package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.TypeAlias
import com.mishaLang.interpreter.Symbol.TypeSymbol

/**
	* This is for both the "width" as well as the "height" property
	*/
object Dimension extends Type {


	override def name: TypeSymbol = "Dimension"

	override def apply(): Type.Any = Type.Union(Set(
		Type.Length,
		Type.Percentage,
		// TODO add "25em border-box" and "75% content-box" and other values
		Type.Literal("max-content"),
		Type.Literal("min-content"),
		Type.Literal("available"),
		Type.Literal("fit-content"),
		Type.Literal("auto"),
		TypeAlias(Global.name)
	))
}
