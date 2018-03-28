package com.mishaLang.spec.types
import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.{Literal, TypeAlias}

object Margin extends Type {


	override def name: String = "Margin"

	override def apply(): Type.Any = Type.Union(Set(
		Type.Length,
		Type.Percentage,
		Literal("auto"),
		TypeAlias(Global.name)
	))
}
