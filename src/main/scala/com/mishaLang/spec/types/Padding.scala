package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.{Literal, TypeAlias}
import com.mishaLang.interpreter.Symbol.TypeSymbol

object Padding extends Type {


	override def name: TypeSymbol = "Padding"

	override def apply(): Type.Any = Type.Union(Set(
		Type.Length,
		Type.Percentage,
		TypeAlias(Global.name)
	))
}
