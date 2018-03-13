package com.preprocessor.emitter

import com.preprocessor.ast.Symbol.RuleContextSymbol

object RuleHeadEmitter {

	def emit(builder: StringBuilder, head: RuleContextSymbol.Value): StringBuilder = head match {
		case _ => builder.append("todo")
	}

}
