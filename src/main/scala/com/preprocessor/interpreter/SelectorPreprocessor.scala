package com.preprocessor.interpreter

class SelectorPreprocessor(val selector: RawRuleHead) {



	def preProcess(): String = {
		RuleHeadPreprocessor.explode(selector)
	}

}
