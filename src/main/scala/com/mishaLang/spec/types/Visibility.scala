package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.{Literal, TypeAlias}

object Visibility extends Type {


	override def name: String = "Visibility"

	override def apply(): Type.Any = Type.Union(Set(
		Literal("visible"),
		Literal("hidden"),
		Literal("collapse"),
		TypeAlias(Global.name)
	))
}
