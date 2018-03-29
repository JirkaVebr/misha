package com.mishaLang.spec.properties
import com.mishaLang.ast.Language.Value

object BoxModel extends Module {


	override def apply(): Map[String, Value.Callable] = Map(
		"height" -> generateUnary("Dimension"),
		// margin
		"margin-bottom" -> generateUnary("Margin"),
		"margin-left" -> generateUnary("Margin"),
		"margin-right" -> generateUnary("Margin"),
		"margin-top" -> generateUnary("Margin"),
		"max-height" -> generateUnary("DimensionLimit"),
		"max-width" -> generateUnary("DimensionLimit"),
		"overflow" -> generateUnary("Overflow"),
		"overflow-x" -> generateUnary("Overflow"),
		"overflow-y" -> generateUnary("Overflow"),
		// overscroll-behavior
		// overscroll-behavior-x
		// overscroll-behavior-y
		// padding
		"padding-bottom" -> generateUnary("Padding"),
		"padding-left" -> generateUnary("Padding"),
		"padding-right" -> generateUnary("Padding"),
		"padding-top" -> generateUnary("Padding"),
		"visibility" -> generateUnary("Visibility"),
		"width" -> generateUnary("Dimension"),
	)
}
