package com.mishaLang.spec.types

import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.{Literal, TypeAlias}

object Padding extends Type {


	override def name: String = "Padding"

	override def apply(): Type.Any = Type.Union(Set(
		Type.Length,
		Type.Percentage,
		TypeAlias(Global.name)
	))
}
