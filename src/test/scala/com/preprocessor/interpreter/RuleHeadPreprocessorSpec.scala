package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.Selector.{Class, Complex, SelectorList}
import com.preprocessor.spec.SelectorCombinator.Descendant

class RuleHeadPreprocessorSpec extends BaseInterpreterSpec {

	behavior of "Rule head preprocessor"

	it should "correctly process raw rule heads" in {
		val rawRuleHead: RawRuleHead = Vector(
			Left(".my"),
			Right(Vector(
				Value.String("class1"),
				Value.String("class2")
			)),
			Left(" .mySubclass "),
			Right(Vector(
				Value.String("div"),
				Value.String("span")
			))
		)

		assert(RuleHeadPreprocessor.explode(rawRuleHead) ===
			".myclass1 .mySubclass div, " +
			".myclass1 .mySubclass span, " +
			".myclass2 .mySubclass div, " +
			".myclass2 .mySubclass span")
	}

}
