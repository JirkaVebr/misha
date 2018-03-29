package com.mishaLang.spec.types
import com.mishaLang.ast.Language.Type
import com.mishaLang.ast.Language.Type.Literal
import com.mishaLang.interpreter.Symbol

object Display extends Type {

	override def name: Symbol.TypeSymbol = "Display"

	override def apply(): Type.Any = Type.Union(Set(
		Literal("block flow"),
		Literal("block"),
		Literal("contents"),
		Literal("flex run-in"),
		Literal("flex"),
		Literal("flow list-item block"),
		Literal("flow"),
		Literal("flow-root"),
		Literal("grid"),
		Literal("inline table"),
		Literal("inline"),
		Literal("inline-block"),
		Literal("inline-flex"),
		Literal("inline-grid"),
		Literal("inline-table"),
		Literal("list-item block flow"),
		Literal("list-item block flow-root"),
		Literal("list-item block"),
		Literal("list-item flow"),
		Literal("list-item flow-root"),
		Literal("list-item inline"),
		Literal("list-item"),
		Literal("none"),
		Literal("ruby"),
		Literal("ruby-base"),
		Literal("ruby-base-container"),
		Literal("ruby-text"),
		Literal("ruby-text-container"),
		Literal("run-in"),
		Literal("subgrid"),
		Literal("table"),
		Literal("table-caption"),
		Literal("table-cell"),
		Literal("table-column"),
		Literal("table-column-group"),
		Literal("table-footer-group"),
		Literal("table-header-group"),
		Literal("table-row"),
		Literal("table-row-group"),
		Global(),
	))
}
