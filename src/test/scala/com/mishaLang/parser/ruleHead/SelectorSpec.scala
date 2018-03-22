package com.mishaLang.parser.ruleHead

import com.mishaLang.ast.Namespace.{AnyNamespace, NamedNamespace}
import com.mishaLang.ast.{MatchTarget, QualifiedAttribute, QualifiedElement}
import com.mishaLang.ast.Selector._
import com.mishaLang.parser.BaseParserSpec
import com.mishaLang.spec.AttributeSelector._
import com.mishaLang.spec.HtmlElements._
import com.mishaLang.spec.PseudoClasses.AnPlusB
import com.mishaLang.spec.SelectorSeparator.{Child, NextSibling}
import com.mishaLang.spec.{PseudoClasses, PseudoElements}
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
		assert(parse(":matches(h1)") === RawSubSelector(PseudoClasses.Matches, Element(QualifiedElement(H1))))
		assert(parse(":not(*)") === RawSubSelector(PseudoClasses.Not, Element(QualifiedElement(AnyElement))))
	}

	it should "correctly parse nth-* pseudo classes" in {
		assert(parse(":nth-of-type(2n + 3)") === RawNth(PseudoClasses.OfType, AnPlusB(2, 3)))
		assert(parse(":nth-child(even of div)") === RawNth(PseudoClasses.Child, AnPlusB(2, 0), Some(Element(QualifiedElement(Div)))))

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

	it should "correctly parse the :lang() pseudo clas" in {
		assert(parse(":lang(en)") === Lang(Set("en")))
		assert(parse(":lang(fr, de, it)") === Lang(Set("fr", "de", "it")))
	}

	it should "correctly parse compound selectors" in {
		assert(parse("div#myId:hover") === RawCompound(Seq(
			Element(QualifiedElement(Div)),
			Id("myId"),
			NonFunctional(PseudoClasses.NonFunctional.Hover)
		)))
	}

	it should "correctly parse complex selectors" in {
		assert(parse("input:focus + .myLabel::before > h1") === RawComplex(NextSibling, RawCompound(Seq(
			Element(QualifiedElement(Input)),
			NonFunctional(PseudoClasses.NonFunctional.Focus)
		)), RawComplex(Child, RawCompound(Seq(
			Class("myLabel"),
			PseudoElement(PseudoElements.Before)
		)), Element(QualifiedElement(H1)))))
	}

	it should "correctly parse selector lists" in {
		assert(parse("h1, h2") === RawSelectorList(Seq(
			Element(QualifiedElement(H1)),
			Element(QualifiedElement(H2))
		)))
		assert(parse("h1::before, h2 + p::after, div") === RawSelectorList(Seq(
			RawCompound(Seq(
				Element(QualifiedElement(H1)),
				PseudoElement(PseudoElements.Before)
			)),
			RawComplex(NextSibling, Element(QualifiedElement(H2)), RawCompound(Seq(
				Element(QualifiedElement(P)),
				PseudoElement(PseudoElements.After)
			))),
			Element(QualifiedElement(Div))
		)))
	}


	protected def parse(input: String): Selector = parseSelectorRule(input, _.Selector)

}
