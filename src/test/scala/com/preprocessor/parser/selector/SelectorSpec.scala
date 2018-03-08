package com.preprocessor.parser.selector

import com.preprocessor.ast.Namespace.AnyNamespace
import com.preprocessor.ast.{MatchTarget, QualifiedAttribute}
import com.preprocessor.ast.Selector._
import com.preprocessor.parser.BaseParserSpec
import com.preprocessor.spec.AttributeSelector._

class SelectorSpec extends BaseParserSpec {

	behavior of "The selector parser"

	it should "correctly parse class names" in {
		assert(parse(".myClass") === Class("myClass"))
	}

	it should "correctly parse ids" in {
		assert(parse("#myId") === Id("myId"))
	}

	it should "correctly parse attributes" in {
		assert(parse("[tabindex]") === Attribute(QualifiedAttribute("tabindex")))
		assert(parse("[|tabindex]") === Attribute(QualifiedAttribute("tabindex")))
		assert(parse("[*|tabindex]") === Attribute(QualifiedAttribute("tabindex", AnyNamespace)))
		assert(parse("[tabindex=123]") === Attribute(QualifiedAttribute("tabindex"), Some(MatchTarget(Equals, "123"))))
		assert(parse("[ 	 |tabindex |=	456	]") === Attribute(QualifiedAttribute("tabindex"), Some(MatchTarget(Prefix, "456"))))
	}


	protected def parse(input: String): Selector = parseSelectorRule(input, _.Selector)

}
