package com.mishaLang.interpreter.validators

import com.mishaLang.ast.Selector._
import com.mishaLang.error.SelectorError
import com.mishaLang.error.SelectorError.{UndefinedElement, UndefinedPseudoElement}
import com.mishaLang.interpreter.EnvWithValue
import com.mishaLang.spec.HtmlElements.CustomElement
import com.mishaLang.spec.PseudoElements.CustomPseudoElement

import scala.util.{Failure, Success, Try}

object SelectorValidator {

	def validateSelector(selector: RawSelector)(implicit state: EnvWithValue): Try[NormalizedSelector] = {
		SelectorNormalizer.normalize(selector) match {
			case Failure(exception) => Failure(exception)
			case Success(normalizedSelector) => validateNormalizedSelector(normalizedSelector)
		}
	}

	def validateNormalizedSelector(selector: NormalizedSelector)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		selector match {
			case complex: ComplexComponent => complex match {
				case compound: CompoundComponent => compound match {
					case simple: SimpleSelector => simple match {
						case subClass: SubClass => subClass match {
							case attribute: Attribute => validateAttribute(attribute)
							case id: Id => validateId(id)
							case normalizedClass: Class => validateClass(normalizedClass)
							case pseudoClass: PseudoClass => validatePseudoClass(pseudoClass)
						}
						case element: Element => validateElement(element)
						case pseudoElement: PseudoElement => validatePseudoElement(pseudoElement)
					}
					case compound: Compound => validateCompound(compound)
				}
				case complex: Complex => validateComplex(complex)
			}
			case selectorList: SelectorList => validateSelectorList(selectorList)
		}


	private def validateSelectorList(selectorList: SelectorList)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		Success(selectorList) // TODO


	private def validateComplex(complex: Complex)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		Success(complex) // TODO


	private def validateCompound(compound: Compound)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		Success(compound) // TODO


	private def validateElement(element: Element)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		element.element.element match { // TODO validate namespace
			case _: CustomElement =>
				Failure(SelectorError(UndefinedElement))
			case _ => Success(element)
		}


	private def validatePseudoElement(element: PseudoElement)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		element.element match {
			case _: CustomPseudoElement =>
				Failure(SelectorError(UndefinedPseudoElement))
			case _ => Success(element)
		}


	private def validateAttribute(attribute: Attribute)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		Success(attribute) // TODO


	private def validateClass(normalizedClass: Class)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		Success(normalizedClass) // There's not much that can go wrong here


	private def validateId(id: Id)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		Success(id) // There's not much that can go wrong here


	private def validatePseudoClass(pseudoClass: PseudoClass)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		Success(pseudoClass) // TODO

}
