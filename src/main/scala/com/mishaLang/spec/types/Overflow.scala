package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.{Literal, TypeAlias}

object Overflow extends Type {


	override def name: String = "Overflow"

	override def apply(): Type.Any = Type.Union(Set(
		Literal("visible"),
		Literal("hidden"),
		Literal("scroll"),
		Literal("auto"),
		TypeAlias(Global.name)
	))
}
