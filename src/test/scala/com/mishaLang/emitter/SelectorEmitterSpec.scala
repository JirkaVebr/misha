package com.mishaLang.emitter

import com.mishaLang.ast.Namespace.{AnyNamespace, NamedNamespace, NoNamespace}
import com.mishaLang.ast.{MatchTarget, QualifiedAttribute, QualifiedElement, Selector}
import com.mishaLang.ast.Selector._
import com.mishaLang.spec.AttributeSelector.Equals
import com.mishaLang.spec.HtmlElements.Div
import com.mishaLang.spec.PseudoClasses.NonFunctional.{FirstChild, Focus, Hover}
import com.mishaLang.spec.PseudoElements.{After, Before}
import com.mishaLang.spec.SelectorSeparator.{Descendant, NextSibling}

class SelectorEmitterSpec extends BaseEmitterSpec {

	behavior of "Selector emitter spec"

	it should "emit elements" in {
		assert(emit(Element(QualifiedElement(Div))) === "div")
		assert(emit(Element(QualifiedElement(Div, Some(AnyNamespace)))) === "*|div")
		assert(emit(Element(QualifiedElement(Div, Some(NamedNamespace("myNamespace"))))) === "myNamespace|div")
		assert(emit(Element(QualifiedElement(Div, Some(NoNamespace)))) === "|div")
	}

	it should "emit pseudo-elements" in {
		assert(emit(PseudoElement(Before)) === "::before")
	}

	it should "emit attributes" in {
		assert(emit(Attribute(QualifiedAttribute("href"))) === "[href]")
		assert(emit(Attribute(QualifiedAttribute("href", AnyNamespace))) === "[*|href]")
		assert(emit(Attribute(QualifiedAttribute("href", NamedNamespace("myNamespace")))) === "[myNamespace|href]")

		assert(emit(Attribute(QualifiedAttribute("href"), Some(MatchTarget(Equals, "foo")))) === "[href=foo]")
		assert(emit(Attribute(
			QualifiedAttribute("href", AnyNamespace),
			Some(MatchTarget(Equals, "foo")))
		) === "[*|href=foo]")
		assert(emit(Attribute(
			QualifiedAttribute("href", NamedNamespace("myNamespace")),
			Some(MatchTarget(Equals, "foo")))
		) === "[myNamespace|href=foo]")

		// TODO attribute values can be quoted strings
		// TODO match targets allow [href i]
	}

	it should "emit ids" in {
		assert(emit(Id("myId")) === "#myId")
	}

	it should "emit classes" in {
		assert(emit(Class("myClass")) === ".myClass")
	}

	it should "emit non-functional pseudo-classes" in {
		assert(emit(NonFunctional(FirstChild)) === ":first-child")
	}

	// TODO test functional pseudo classes

	it should "emit compound selectors" in {
		val element = Element(QualifiedElement(Div))
		val class1 = Class("myClass1")
		val class2 = Class("myClass2")
		val id = Id("myId")
		val focus = NonFunctional(Focus)
		val hover = NonFunctional(Hover)
		val after = Selector.PseudoElement(After)

		assert(emit(Compound(
			Some(element),
			Set(class1, class2, id, focus),
			Some(after),
			Set(hover)
		)) === "div.myClass1.myClass2#myId:focus::after:hover")

		assert(emit(Compound(
			None,
			Set(class1, class2, id, focus),
			Some(after),
			Set(hover)
		)) === ".myClass1.myClass2#myId:focus::after:hover")

		assert(emit(Compound(
			Some(element),
			Set(class1, class2, id, focus),
			None,
			Set(hover)
		)) === "div.myClass1.myClass2#myId:focus:hover")
	}

	it should "emit complex selectors" in {
		assert(emit(Complex(
			Descendant,
			Element(QualifiedElement(Div)),
			Complex(
				NextSibling,
				Class("myClass1"),
				Class("myClass2")
			)
		)) === "div .myClass1 + .myClass2")
	}

	it should "emit selector lists" in {
		assert(emit(SelectorList(Set(
			Class("myClass1"),
			Class("myClass2"),
			Element(QualifiedElement(Div))
		))) === ".myClass1, .myClass2, div")
	}


	def emit(selector: NormalizedSelector): String =
		SelectorEmitter.emit(selector)(StringBuilder.newBuilder).toString

}
