package com.preprocessor.parser.selector

import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace}
import com.preprocessor.ast.{MatchTarget, QualifiedAttribute, QualifiedElement}
import com.preprocessor.ast.Selector._
import com.preprocessor.parser.BaseParserSpec
import com.preprocessor.spec.AttributeSelector._
import com.preprocessor.spec.HtmlElements.{AnyElement, CustomElement, Div, H1}
import com.preprocessor.spec.PseudoElements

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

	it should "correctly parse elements" in {
		assert(parse("div") === Element(QualifiedElement(Div)))
		assert(parse("h1") === Element(QualifiedElement(H1)))
		assert(parse("*") === Element(QualifiedElement(AnyElement)))
		assert(parse("*|*") === Element(QualifiedElement(AnyElement, Some(AnyNamespace))))
		assert(parse("someRandomElement") === Element(QualifiedElement(CustomElement("someRandomElement"))))
		assert(parse("myNamespace|someRandomElement") === Element(
			QualifiedElement(CustomElement("someRandomElement"), Some(NamedNamespace("myNamespace")))
		))
	}

	it should "correctly parse pseudo elements" in {
		assert(parse("::before") === PseudoElement(PseudoElements.Before))
		assert(parse("::selection") === PseudoElement(PseudoElements.Selection))
		assert(parse("::balderdash") === PseudoElement(PseudoElements.CustomPseudoElement("balderdash")))
	}


	protected def parse(input: String): Selector = parseSelectorRule(input, _.Selector)

}
