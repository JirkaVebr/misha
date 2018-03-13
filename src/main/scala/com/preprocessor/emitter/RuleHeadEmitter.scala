package com.preprocessor.emitter

import com.preprocessor.ast.Symbol.RuleContextSymbol

object RuleHeadEmitter {

	def emit(head: RuleContextSymbol.Value)(implicit builder: StringBuilder): StringBuilder = head match {
		case _ => builder.append("todo")
	}

}
