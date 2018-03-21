package com.preprocessor.interpreter.validators

import com.preprocessor.ast.Selector
import com.preprocessor.ast.Selector._
import com.preprocessor.error.SelectorError
import com.preprocessor.error.SelectorError._

import scala.util.{Failure, Success, Try}

object SelectorNormalizer {

	def normalize(selector: Selector): Try[NormalizedSelector] = selector match {
		case raw: RawSelector => raw match {
			case rawSelectorList: RawSelectorList => normalizeSelectorList(rawSelectorList)
			case complexComponent: RawComplexComponent => normalizeRawComplexComponent(complexComponent)
		}
		case normalized: NormalizedSelector => normalizeNormalized(normalized)
	}


	private def normalizeRawComplexComponent(complexComponent: RawComplexComponent): Try[ComplexComponent] =
		complexComponent match {
			case rawComplex: RawComplex =>
				normalizeRawComplex(rawComplex)
			case compoundComponent: RawCompoundComponent => compoundComponent match {
				case rawCompound: RawCompound =>
					RawCompoundNormalizer.normalize(rawCompound)
				case simple: RawSimpleSelector => simple match {
					case subClass: RawSubClass => subClass match {
						case pseudoClass: RawPseudoClass => pseudoClass match {
							case RawSubSelector(kind, subSelector) => normalize(subSelector) match {
								case Failure(exception) => Failure(exception)
								case Success(normalized) => Success(SubSelector(kind, normalized))
							}
							case RawNth(kind, ab, of) => of match {
								case Some(ofSelector) => normalize(ofSelector) match {
									case Failure(exception) => Failure(exception)
									case Success(normalizedSelector) => Success(Nth(kind, ab, Some(normalizedSelector)))
								}
								case None => Success(Nth(kind, ab))
							}

							// Silly compiler… Can't use "_"
							case normalized: NonFunctional => normalizeComplexComponent(normalized)
							case normalized: Dir => normalizeComplexComponent(normalized)
							case normalized: Drop => normalizeComplexComponent(normalized)
							case normalized: Lang => normalizeComplexComponent(normalized)
						}
						case normalized: Class => normalizeComplexComponent(normalized)
						case normalized: Attribute => normalizeComplexComponent(normalized)
						case normalized: Selector.Id => normalizeComplexComponent(normalized)
					}
					case normalized: Element => normalizeComplexComponent(normalized)
					case normalized: PseudoElement => normalizeComplexComponent(normalized)
				}
			}
		}


	private def normalizeNormalized(selector: NormalizedSelector): Try[NormalizedSelector] =
		Success(selector)


	private def normalizeComplexComponent(selector: ComplexComponent): Try[ComplexComponent] =
		Success(selector)


	// TODO validate illegal combinator use
	private def normalizeRawComplex(rawComplex: RawComplex): Try[Complex] =
		chainNormalizeRawComplex(rawComplex.left :: rawComplex.right :: Nil) match {
			case Failure(exception) =>
				Failure(exception)
			case Success(normalizedLeft :: normalizedRight :: Nil) =>
				Success(Complex(rawComplex.combinator, normalizedLeft, normalizedRight))
			case _ => sys.error("this shouldn't happen") // TODO
		}


	private def normalizeSelectorList(list: RawSelectorList): Try[SelectorList] =
		chainNormalizeRawComplex(list.selectors) match {
			case Failure(exception) => Failure(exception)
			case Success(normalizedSelectors) =>
				val normalizedSet = normalizedSelectors.toSet

				if (list.selectors.lengthCompare(normalizedSet.size) > 0)
					Failure(SelectorError(DuplicateSelectorInList))
				else
					Success(SelectorList(normalizedSet))
		}


	def chainNormalizeRawComplex(selectors: Seq[RawComplexComponent]): Try[Seq[ComplexComponent]] =
		if (selectors.isEmpty) Success(Seq.empty[ComplexComponent])
		else {
			normalizeRawComplexComponent(selectors.head) match {
				case Failure(exception) => Failure(exception)
				case Success(normalizedHead) => chainNormalizeRawComplex(selectors.tail) match {
					case Failure(exception) => Failure(exception)
					case Success(normalizedTail) => Success(normalizedHead +: normalizedTail)
				}
			}
		}

}
