package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Term.ParentSelector
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

		assert(RuleHeadPreprocessor.explode(rawRuleHead) ===
			""".myclass1 .mySubclass div,
				|.myclass1 .mySubclass span,
				|.myclass2 .mySubclass div,
				|.myclass2 .mySubclass span""".stripMargin)
	}

	it should "correctly prepend implicit parent selectors" in {
		assert(RuleHeadPreprocessor.prependImplicitParent(
			Vector(Left(".myClass"))
		) === Vector(
			Right(Vector(ParentSelector)),
			Left(" "),
			Left(".myClass")
		))

		assert(RuleHeadPreprocessor.prependImplicitParent(
			Vector(
				Right(Vector(ParentSelector)),
				Left("-active")
			)
		) === Vector(
			Right(Vector(ParentSelector)),
			Left("-active")
		))
	}

}
