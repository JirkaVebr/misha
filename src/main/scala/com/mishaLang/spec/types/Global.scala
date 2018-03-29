package com.mishaLang.spec.types
import com.mishaLang.ast.Language.Type
import com.mishaLang.interpreter.Symbol.TypeSymbol

object Global extends Type {

	override def name: TypeSymbol = "Global"

	override def apply(): Type.Any = Type.Union(Set(
		Type.Literal("inherit"),
		Type.Literal("initial"),
		Type.Literal("unset"),
	))
}
