package com.mishaLang.ast.atRules

import com.mishaLang.interpreter.Symbol.ValueSymbol

object Keyframes {

	case class KeyframeRule(identifier: ValueSymbol)


	sealed trait KeyframeSelector

	case object From extends KeyframeSelector
	case object To extends KeyframeSelector
	case class Percentage(value: Double) extends KeyframeSelector
}
