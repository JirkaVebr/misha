package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Value

class SelectorPreprocessor(val selector: RawRuleHead, val parentSelector: Vector[Value.String]) {



	def preProcess(): String = {
		RuleHeadPreprocessor.explode(selector) // TODO this is super naïve & temporary
	}

}
