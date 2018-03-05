package com.preprocessor.parser


import org.parboiled2._


class LanguageParser(val input: IndentDedentParserInput) extends org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Literals
	with L4_Types
	with L5_Expressions
	with L6_TopLevel


object LanguageParser {

	def create(input: String): LanguageParser =
		new LanguageParser(new IndentDedentParserInput(input))
}
