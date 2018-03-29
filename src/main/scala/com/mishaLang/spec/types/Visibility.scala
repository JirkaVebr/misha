package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.{Literal, TypeAlias}
import com.mishaLang.interpreter.Symbol.TypeSymbol

object Visibility extends Type {


	override def name: TypeSymbol = "Visibility"

	override def apply(): Type.Any = Type.Union(Set(
		Literal("visible"),
		Literal("hidden"),
		Literal("collapse"),
		TypeAlias(Global.name)
	))
}
