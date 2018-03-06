package com.preprocessor.parser.language

import com.preprocessor.ast.Language.Program
import com.preprocessor.parser._
import com.preprocessor.parser.common.{L0_Whitespace, L1_Strings, L2_Numbers}
import org.parboiled2._

import scala.util.Try


class LanguageParser(val input: IndentDedentParserInput) extends org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Literals
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
