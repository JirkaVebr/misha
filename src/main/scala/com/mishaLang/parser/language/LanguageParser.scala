package com.mishaLang.parser.language

import com.mishaLang.ast.Language.Program
import com.mishaLang.parser._
import com.mishaLang.parser.common.{L0_Whitespace, L1_AstNode, L2_Strings, L3_Numbers}
import org.parboiled2._

import scala.util.Try


class LanguageParser(val input: IndentDedentParserInput) extends org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_AstNode
	with L2_Strings
	with L3_Numbers
	with L3_PrimitiveLiterals
	with L4_Types
	with L5_Expressions
	with L6_TopLevel


object LanguageParser extends ParserFactory[LanguageParser]
	with ParserOf[Program] {

	override def create(input: String): LanguageParser =
		new LanguageParser(new IndentDedentParserInput(input))

	override def apply(input: String): Try[Program] =
		create(input).Program.run()
}
