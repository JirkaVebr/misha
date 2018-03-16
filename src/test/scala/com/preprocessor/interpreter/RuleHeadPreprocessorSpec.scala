package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Value

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
		val ruleHeadPreprocessor = new RuleHeadPreprocessor(rawRuleHead, testEnvironment)

		assert(ruleHeadPreprocessor.preProcess() ===
			""".myclass1 .mySubclass div,
				|.myclass1 .mySubclass span,
				|.myclass2 .mySubclass div,
				|.myclass2 .mySubclass span""".stripMargin)
	}

}
