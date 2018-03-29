package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.{Literal, TypeAlias}
import com.mishaLang.interpreter.Symbol.TypeSymbol

object Overflow extends Type {


	override def name: TypeSymbol = "Overflow"

	override def apply(): Type.Any = Type.Union(Set(
		Literal("visible"),
		Literal("hidden"),
		Literal("scroll"),
		Literal("auto"),
		TypeAlias(Global.name)
	))
}
