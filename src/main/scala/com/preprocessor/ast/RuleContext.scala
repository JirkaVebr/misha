package com.preprocessor.ast

import com.preprocessor.ast.Selector.Selector

object RuleContext {

	sealed trait RuleContext

	abstract class ProcessedRuleHead(val originalHead: String) extends RuleContext


	case class RuleSelector(selector: Selector, override val originalHead: String) extends ProcessedRuleHead(originalHead)

	/*sealed trait AtRule extends ProcessedRuleHead

	case class CounterStyle() extends AtRule
	case class Document() extends AtRule
	case class FontFace() extends AtRule
	case class FontFeatureValues() extends AtRule
	case class Keyframes() extends AtRule
	case class Media() extends AtRule
	case class Page() extends AtRule
	case class Supports() extends AtRule
	case class Viewport() extends AtRule*/


}
