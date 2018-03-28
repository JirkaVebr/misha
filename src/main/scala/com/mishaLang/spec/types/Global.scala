package com.mishaLang.spec.types
import com.mishaLang.ast.Language.Type

object Global extends Type {

	override def name: String = "Global"

	override def apply(): Type.Any = Type.Union(Set(
		Type.Literal("inherit"),
		Type.Literal("initial"),
		Type.Literal("unset"),
	))
}
