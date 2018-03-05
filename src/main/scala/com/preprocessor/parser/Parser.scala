package com.preprocessor.parser


import org.parboiled2._


class Parser(val input: IndentDedentParserInput) extends org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Literals
	with L3_Types
	with L4_Expressions
	with L6_TopLevel


object Parser {

	def create(input: String): Parser =
		new Parser(new IndentDedentParserInput(input))
}
