package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.TypeAlias

object Height extends Type {


	override def name: String = "Height"

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
