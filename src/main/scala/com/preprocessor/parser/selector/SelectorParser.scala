package com.preprocessor.parser.selector

import com.preprocessor.ast.Selector.Selector
import com.preprocessor.parser.common.{L0_Whitespace, L1_Strings, L2_Numbers}
import com.preprocessor.parser.{ParserFactory, ParserOf}
import org.parboiled2.ParserInput.StringBasedParserInput
import org.parboiled2._

import scala.util.Try

class SelectorParser(val input: ParserInput) extends Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Basics
	with L4_AnPlusB
	with L5_Selector {

}

object SelectorParser extends ParserFactory[SelectorParser]
	with ParserOf[Selector] {

	override def create(input: String): SelectorParser =
		new SelectorParser(new StringBasedParserInput(input))

	override def apply(input: String): Try[Selector] =
		create(input).Selector.run()
}
