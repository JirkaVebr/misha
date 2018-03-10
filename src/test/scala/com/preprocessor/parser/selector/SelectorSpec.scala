package com.preprocessor.parser.selector

import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace}
import com.preprocessor.ast.{MatchTarget, QualifiedAttribute, QualifiedElement}
import com.preprocessor.ast.Selector._
import com.preprocessor.parser.BaseParserSpec
import com.preprocessor.spec.AttributeSelector._
import com.preprocessor.spec.HtmlElements.{AnyElement, CustomElement, Div, H1}
import com.preprocessor.spec.PseudoClasses.AnPlusB
import com.preprocessor.spec.{PseudoClasses, PseudoElements}
import org.parboiled2.ParseError

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

	it should "correctly parse non-functional pseudo classes" in {
		assert(parse(":hover") === NonFunctional(PseudoClasses.NonFunctional.Hover))
		assert(parse(":focus") === NonFunctional(PseudoClasses.NonFunctional.Focus))
		assert(parse(":random-pseudo-class") === NonFunctional(PseudoClasses.NonFunctional.CustomPseudoClass("random-pseudo-class")))
	}

	it should "correctly parse sub-selectors" in {
		assert(parse(":matches(h1)") === SubSelector(PseudoClasses.Matches, Element(QualifiedElement(H1))))
		assert(parse(":not(*)") === SubSelector(PseudoClasses.Not, Element(QualifiedElement(AnyElement))))
	}

	it should "correctly parse nth-* pseudo classes" in {
		assert(parse(":nth-of-type(2n + 3)") === Nth(PseudoClasses.OfType, AnPlusB(2, 3)))
		assert(parse(":nth-child(even of div)") === Nth(PseudoClasses.Child, AnPlusB(2, 0), Some(Element(QualifiedElement(Div)))))

		assertThrows[ParseError](parse(":nth-of-type(2nn)"))
	}

	it should "correctly parse the :dir() pseudo class" in {
		assert(parse(":dir(ltr)") === Dir(PseudoClasses.Ltr))
		assert(parse(":dir(from-the-top-to-the-bottom)") === Dir(PseudoClasses.UndefinedDirectionality("from-the-top-to-the-bottom")))
	}

	it should "correctly parse the :drop() pseudo class" in {
		assert(parse(":drop") === NonFunctional(PseudoClasses.NonFunctional.Drop))
		assert(parse(":drop()") === NonFunctional(PseudoClasses.NonFunctional.Drop))
		assert(parse(":drop(active)") === Drop(Set(PseudoClasses.Active)))
		assert(parse(":drop(invalid active)") === Drop(Set(PseudoClasses.Active, PseudoClasses.Invalid)))

		assertThrows[ParseError](parse(":drop(invalid invalid)"))
		assertThrows[ParseError](parse(":drop(something-invalid)"))
	}


	protected def parse(input: String): Selector = parseSelectorRule(input, _.Selector)

}
