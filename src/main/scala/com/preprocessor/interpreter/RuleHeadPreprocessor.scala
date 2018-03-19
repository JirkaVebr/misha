package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Term.ParentSelector
import com.preprocessor.ast.RuleHead
import com.preprocessor.spec.SelectorCombinator


/**
	* A rule head can look something like this:
	*     .foo-{1 + 1, 3 + 3}-bar
	* Which, in the end, should generate
	*     .foo-2-bar, .foo-6-bar
	*
	*/
object RuleHeadPreprocessor {

	def explode(rawRuleHead: RawRuleHead): String = {
		def rec(ruleHead: Vector[RawRuleHeadComponent]): Vector[String] =
			if (ruleHead.isEmpty)
				Vector.empty
			else {
				val (head, rest) = (ruleHead.head, rec(ruleHead.tail))
				head match {
					case Left(string) =>
						if (rest.isEmpty) Vector(string)
						else rest.map(string + _)
					case Right(values) =>
						values.flatMap(
							stringValue =>
								if (rest.isEmpty) Vector(stringValue.value)
								else rest.map(stringValue.value + _)
						)
				}
			}

		rec(rawRuleHead).mkString(",\n").trim
	}


	def prependImplicitParent(ruleHead: RuleHead): RuleHead = {
		lazy val implicitParent = Vector(Right(Vector(ParentSelector)), Left(SelectorCombinator.Descendant.symbol))

		ruleHead.head match {
			case Left(_) => implicitParent ++ ruleHead
			case Right(expressions) =>
				if (expressions == Vector(ParentSelector)) ruleHead
				else implicitParent ++ ruleHead
		}
	}

}
