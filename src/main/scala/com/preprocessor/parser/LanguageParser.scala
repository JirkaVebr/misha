package com.preprocessor.parser


import org.parboiled2._


class LanguageParser(val input: IndentDedentParserInput) extends org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Literals
	with L3_Types
	with L4_Expressions
	with L5_TopLevel


object LanguageParser {

	def create(input: String): LanguageParser =
		new LanguageParser(new IndentDedentParserInput(input))
}
