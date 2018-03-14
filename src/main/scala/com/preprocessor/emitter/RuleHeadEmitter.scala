package com.preprocessor.emitter

import com.preprocessor.ast.RuleContext.RuleSelector
import com.preprocessor.ast.Symbol.RuleContextSymbol

object RuleHeadEmitter {

	def emit(head: RuleContextSymbol.Value)(implicit builder: StringBuilder): StringBuilder = head match {
		case RuleSelector(selector) => SelectorEmitter.emit(selector)
		case _ => sys.error("todo") // TODO
	}

}
