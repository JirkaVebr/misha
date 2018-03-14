package com.preprocessor.interpreter.validators

import com.preprocessor.ast.{QualifiedElement, Selector}
import com.preprocessor.ast.Selector._
import com.preprocessor.error.SelectorError
import com.preprocessor.interpreter.{BaseInterpreterSpec, Environment}
import com.preprocessor.spec.HtmlElements.{Div, Span}
import com.preprocessor.spec.PseudoClasses.NonFunctional.{Focus, Hover}
import com.preprocessor.spec.PseudoElements.After

import scala.util.{Failure, Success}

class SelectorNormalizerSpec extends BaseInterpreterSpec {

	implicit val environment: Environment = testEnvironment

	behavior of "Selector normalizer"

	it should "normalize valid compound selectors" in {
		val element = Element(QualifiedElement(Div))
		val class1 = Class("myClass1")
		val class2 = Class("myClass2")
		val id = Id("myId")
		val focus = NonFunctional(Focus)
		val hover = NonFunctional(Hover)
		val after = Selector.PseudoElement(After)
		assert(normalize(RawCompound(Seq(
			element, class1, id, focus, class2, after, hover
		))) === Compound(
			Some(element),
			Set(class1, class2, id, focus),
			Some(after),
			Set(hover)
		))
	}

	it should "reject duplicate id selectors within compound selectors" in {
		assertThrows[SelectorError](normalize(RawCompound(Seq(Id("id1"), Class("myClass"), Id("id2")))))
	}

	it should "reject duplicate type selectors within compound selectors" in {
		assertThrows[SelectorError](normalize(RawCompound(Seq(
			Element(QualifiedElement(Div)),
			Element(QualifiedElement(Span))
		))))
	}

	it should "normalize selector lists" in {
		val element = Element(QualifiedElement(Div))
		val class1 = Class("myClass1")

		assert(normalize(RawSelectorList(Seq(
			element, class1
		))) === SelectorList(Set(
			element, class1
		)))
	}

	it should "reject selector lists with duplicates" in {
		val element = Element(QualifiedElement(Div))
		val class1 = Class("myClass1")

		assertThrows[SelectorError](normalize(RawSelectorList(Seq(
			element, class1, class1
		))))
	}


	private def normalize(selector: Selector): NormalizedSelector =
		SelectorNormalizer.normalize(selector) match {
			case Failure(exception) => throw exception
			case Success(normalized) => normalized
		}
}
