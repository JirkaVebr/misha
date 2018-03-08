package com.preprocessor.parser.selector

import com.preprocessor.ast.Selector._
import com.preprocessor.parser.BaseParserSpec

class SelectorSpec extends BaseParserSpec {

	behavior of "The selector parser"

	it should "correctly parse class names" in {
		assert(parse(".myClass") === Class("myClass"))
	}

	it should "correctly parse ids" in {
		assert(parse("#myId") === Id("myId"))
	}


	protected def parse(input: String): Selector = parseSelectorRule(input, _.Selector)

}
