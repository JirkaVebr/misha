package com.preprocessor.interpreter


/**
	* A rule head can look something like this:
	*     .foo-{1 + 1, 3 + 3}-bar
	* Which, in the end, should generate
	*     .foo-2-bar, .foo-6-bar
	*
	*
	* Warning:
	*
	*/
object RuleHeadPreprocessor {


	def preProcess(rawRuleHead: RawRuleHead): String = // TODO
		rawRuleHead.foldLeft(StringBuilder.newBuilder)({
			case (builder, component) => component match {
				case Left(string) => builder.append(string)
				case Right(strings) => builder.append(strings.map(_.value).mkString(","))
			}
		}).toString

}
