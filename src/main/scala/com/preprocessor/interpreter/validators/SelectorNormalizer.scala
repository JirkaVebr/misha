package com.preprocessor.interpreter.validators

import com.preprocessor.ast.Selector._
import com.preprocessor.error.SelectorError
import com.preprocessor.error.SelectorError._
import com.preprocessor.interpreter.Environment
import shapeless.Poly1

import scala.util.{Failure, Success, Try}

object SelectorNormalizer {

	def normalize(selector: Selector)(implicit environment: Environment): Try[NormalizedSelector] = selector match {
		case raw: RawSelector => raw match {
			case rawSelectorList: RawSelectorList => normalizeSelectorList(rawSelectorList)
			case complexComponent: RawComplexComponent => complexComponent match {
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
								case normalized: NonFunctional => normalizeNormalized(normalized)
								case normalized: Dir => normalizeNormalized(normalized)
								case normalized: Drop => normalizeNormalized(normalized)
								case normalized: Lang => normalizeNormalized(normalized)
							}
							case normalized: Class => normalizeNormalized(normalized)
							case normalized: Attribute => normalizeNormalized(normalized)
							case normalized: Id => normalizeNormalized(normalized)
						}
						case normalized: Element => normalizeNormalized(normalized)
						case normalized: PseudoElement => normalizeNormalized(normalized)
					}
				}
			}
		}
		case normalized: NormalizedSelector => Success(normalized)
	}


	private def normalizeNormalized(selector: NormalizedSelector)(implicit environment: Environment): Try[NormalizedSelector] =
		Success(selector)


	// TODO validate illegal combinator use
	private def normalizeRawComplex(rawComplex: RawComplex)(implicit environment: Environment): Try[Complex] =
		chainNormalize(rawComplex.left :: rawComplex.right :: Nil) match {
			case Failure(exception) =>
				Failure(exception)
			case Success(normalizedLeft :: normalizedRight :: Nil) =>
				//Success(Complex(rawComplex.combinator, normalizedLeft, normalizedRight))
				Success(Complex(rawComplex.combinator, Class(""), Class(""))) // TODO!!!!
			case _ => sys.error("this shouldn't happen") // TODO
		}


	private def normalizeSelectorList(list: RawSelectorList)(implicit environment: Environment): Try[SelectorList] =
		chainNormalize(list.selectors) match {
			case Failure(exception) => Failure(exception)
			case Success(normalizedSelectors) =>
				val normalizedSet = normalizedSelectors.toSet

				if (list.selectors.lengthCompare(normalizedSet.size) > 0)
					Failure(SelectorError(DuplicateSelectorInList))
				else
					//Success(SelectorList(normalizedSet))
					Success(SelectorList(Set())) // TODO!!!
		}


	def chainNormalize(selectors: Seq[Selector])(implicit environment: Environment): Try[Seq[NormalizedSelector]] =
		if (selectors.isEmpty) Success(Seq.empty[NormalizedSelector])
		else {
			normalize(selectors.head) match {
				case Failure(exception) => Failure(exception)
				case Success(normalizedHead) => chainNormalize(selectors.tail) match {
					case Failure(exception) => Failure(exception)
					case Success(normalizedTail) => Success(normalizedHead +: normalizedTail)
				}
			}
		}


	//object staticChainNormalize extends Poly1 {
	//	implicit def caseRawComplex
	//}

}
