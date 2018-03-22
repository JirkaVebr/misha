package com.mishaLang.emitter

import com.mishaLang.interpreter.RuleContext.RuleSelector
import com.mishaLang.interpreter.Symbol.RuleContextSymbol

object RuleHeadEmitter {

	def emit(head: RuleContextSymbol.Value)(implicit builder: StringBuilder): StringBuilder = head match {
		case RuleSelector(selector) => SelectorEmitter.emit(selector)
		case _ => sys.error("todo") // TODO
	}

}
