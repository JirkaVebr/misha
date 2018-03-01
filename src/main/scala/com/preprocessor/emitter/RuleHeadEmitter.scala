package com.preprocessor.emitter

import com.preprocessor.ast.Ast.Term.MagicSymbol
import com.preprocessor.ast.RuleContext.RawRuleHead
import com.preprocessor.ast.Symbol.RuleContextSymbol

object RuleHeadEmitter {

	def emit(builder: StringBuilder, head: RuleContextSymbol.Value): StringBuilder = head match {
		case RawRuleHead(components) => components.foldLeft(builder)( // TODO
			(intermediateBuilder: StringBuilder, component: Either[MagicSymbol, String]) =>
				if (component.isRight) intermediateBuilder.append(component.right.get)
				else intermediateBuilder
			)
		case _ => builder.append("todo")
	}

}
