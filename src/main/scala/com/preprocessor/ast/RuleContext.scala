package com.preprocessor.ast

import com.preprocessor.ast.Selector.Selector

object RuleContext {

	sealed trait RuleContext

	case class RuleSelector(selector: Selector) extends RuleContext


	sealed trait AtRule extends RuleContext

	case class CounterStyle() extends AtRule
	case class Document() extends AtRule
	case class FontFace() extends AtRule
	case class FontFeatureValues() extends AtRule
	case class Keyframes() extends AtRule
	case class Media() extends AtRule
	case class Page() extends AtRule
	case class Supports() extends AtRule
	case class Viewport() extends AtRule


}
