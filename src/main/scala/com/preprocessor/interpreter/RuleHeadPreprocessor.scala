package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Value


/**
	* A rule head can look something like this:
	*     .foo-{1 + 1, 3 + 3}-bar
	* Which, in the end, should generate
	*     .foo-2-bar, .foo-6-bar
	*
	*/
object RuleHeadPreprocessor {

	def explode(rawRuleHead: RawRuleHead, separator: String = ", "): String = {
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

		rec(rawRuleHead).mkString(separator).trim
	}

	def singleLevelExplode(values: Vector[Value.String], rest: RawRuleHead): Vector[RawRuleHead] =
		values.map(
			stringValue =>
				Left(stringValue.value) +: rest
		)

	def singleLevelExplode(values: Right[String, Vector[Value.String]], rest: RawRuleHead): Vector[RawRuleHead] =
		singleLevelExplode(values.right.get, rest)



}
