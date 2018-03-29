package com.mishaLang.spec.types
import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.{Literal, TypeAlias}
import com.mishaLang.interpreter.Symbol.TypeSymbol

object Margin extends Type {


	override def name: TypeSymbol = "Margin"

	override def apply(): Type.Any = Type.Union(Set(
		Type.Length,
		Type.Percentage,
		Literal("auto"),
		TypeAlias(Global.name)
	))
}
