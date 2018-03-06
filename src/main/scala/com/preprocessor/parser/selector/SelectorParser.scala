package com.preprocessor.parser.selector

import com.preprocessor.ast.RuleContext.Selector.Selector
import com.preprocessor.parser.ParserFactory
import com.preprocessor.parser.common.{L0_Whitespace, L1_Strings, L2_Numbers}
import org.parboiled2.ParserInput.StringBasedParserInput
import org.parboiled2._

import scala.util.Try

class SelectorParser(val input: ParserInput) extends org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_AnPlusB {

	def Selector: Rule1[Selector] = ???

}

object SelectorParser extends ParserFactory[SelectorParser] {

	override def create(input: String): SelectorParser =
		new SelectorParser(new StringBasedParserInput(input))

	def parse(input: String): Try[Selector] =
		create(input).Selector.run()
}
