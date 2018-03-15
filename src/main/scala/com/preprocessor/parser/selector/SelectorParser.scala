package com.preprocessor.parser.selector

import com.preprocessor.ast.Selector.Selector
import com.preprocessor.parser.common.{L0_Whitespace, L1_AstNode, L2_Strings, L3_Numbers}
import com.preprocessor.parser.{ParserFactory, ParserOf}
import org.parboiled2.ParserInput.StringBasedParserInput
import org.parboiled2._

import scala.util.Try

class SelectorParser(val input: ParserInput) extends Parser
	with StringBuilding
	with L0_Whitespace
	with L1_AstNode
	with L2_Strings
	with L3_Numbers
	with L4_Basics
	with L5_AnPlusB
	with L6_Selector {

}

object SelectorParser extends ParserFactory[SelectorParser]
	with ParserOf[Selector] {

	override def create(input: String): SelectorParser =
		new SelectorParser(new StringBasedParserInput(input))

	override def apply(input: String): Try[Selector] =
		create(input).Selector.run()
}
