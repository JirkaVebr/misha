package com.mishaLang.interpreter.validators

import com.mishaLang.ast.QualifiedElement
import com.mishaLang.ast.Selector.{Element, NormalizedSelector, PseudoElement}
import com.mishaLang.error.SelectorError
import com.mishaLang.interpreter.BaseInterpreterSpec
import com.mishaLang.spec.HtmlElements.CustomElement
import com.mishaLang.spec.PseudoElements.CustomPseudoElement

class SelectorValidatorSpec extends BaseInterpreterSpec {

	behavior of "Selector validator"

	it should "reject unknown elements" in {
		assertThrows[SelectorError](validate(
			Element(QualifiedElement(CustomElement("unknownElement")))
		))
	}

	it should "reject unknown pseudo-elements" in {
		assertThrows[SelectorError](validate(
			PseudoElement(CustomPseudoElement("unknownPseudoElement"))
		))
	}


	private def validate(selector: NormalizedSelector): NormalizedSelector =
		SelectorValidator.validateNormalizedSelector(selector).get

}
