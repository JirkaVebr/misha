package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Value

class SelectorPreprocessor(val selector: RawRuleHead, val parentSelector: Vector[Value.String]) {



	def preProcess(): String = {
		RuleHeadPreprocessor.explode(selector) // TODO this is super naïve & temporary
	}

}
