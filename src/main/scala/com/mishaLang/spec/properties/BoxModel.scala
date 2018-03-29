package com.mishaLang.spec.properties
import com.mishaLang.ast.Language.Value
import com.mishaLang.interpreter.Symbol.ValueSymbol

object BoxModel extends Module {


	override def apply(): Map[ValueSymbol, Value.Callable] = Map(
		ValueSymbol("height") -> generateUnary("height", "Dimension"),
		// margin
		ValueSymbol("margin-bottom") -> generateUnary("margin-bottom", "Margin"),
		ValueSymbol("margin-left") -> generateUnary("margin-left", "Margin"),
		ValueSymbol("margin-right") -> generateUnary("margin-right", "Margin"),
		ValueSymbol("margin-top") -> generateUnary("margin-top", "Margin"),
		ValueSymbol("max-height") -> generateUnary("max-height", "DimensionLimit"),
		ValueSymbol("max-width") -> generateUnary("max-width", "DimensionLimit"),
		ValueSymbol("overflow") -> generateUnary("overflow", "Overflow"),
		ValueSymbol("overflow-x") -> generateUnary("overflow-x", "Overflow"),
		ValueSymbol("overflow-y") -> generateUnary("overflow-y", "Overflow"),
		// overscroll-behavior
		// overscroll-behavior-x
		// overscroll-behavior-y
		// padding
		ValueSymbol("padding-bottom") -> generateUnary("padding-bottom", "Padding"),
		ValueSymbol("padding-left") -> generateUnary("padding-left", "Padding"),
		ValueSymbol("padding-right") -> generateUnary("padding-right", "Padding"),
		ValueSymbol("padding-top") -> generateUnary("padding-top", "Padding"),
		ValueSymbol("visibility") -> generateUnary("visibility", "Visibility"),
		ValueSymbol("width") -> generateUnary("width", "Dimension"),
	)
}
