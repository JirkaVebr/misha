package com.preprocessor.emitter

import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace, NoNamespace}
import com.preprocessor.ast.{MatchTarget, QualifiedAttribute, QualifiedElement}
import com.preprocessor.ast.Selector._
import com.preprocessor.spec.AttributeSelector.Equals
import com.preprocessor.spec.HtmlElements.Div
import com.preprocessor.spec.PseudoClasses.NonFunctional.FirstChild
import com.preprocessor.spec.PseudoElements.Before

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


	def emit(selector: NormalizedSelector): String =
		SelectorEmitter.emit(selector)(StringBuilder.newBuilder).toString

}
