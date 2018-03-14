package com.preprocessor.interpreter.validators

import com.preprocessor.ast.Selector._
import com.preprocessor.error.SelectorError._
import com.preprocessor.error.{CompilerError, SelectorError}
import com.preprocessor.interpreter.Environment

import scala.util.{Failure, Success, Try}

object SelectorNormalizer {

	def normalize(selector: Selector)(implicit environment: Environment): Try[NormalizedSelector] = selector match {
		case raw: RawSelector => raw match {
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
			case rawCompound: RawCompound =>
				normalizeRawCompound(rawCompound)
			case rawComplex: RawComplex =>
				normalizeRawComplex(rawComplex)
			case rawSelectorList: RawSelectorList =>
				normalizeSelectorList(rawSelectorList)
		}
		case normalized: NormalizedSelector => Success(normalized) // TODO validate
	}


	private def normalizeRawCompound(rawCompound: RawCompound)(implicit environment: Environment): Try[Compound] = {
		chainNormalize(rawCompound.selectors) match {
			case Failure(exception) => Failure(exception)
			case Success(selectors) => // Assuming there are at least two selectors
				type Components = (Option[Element], Option[Id], Set[SimpleSelector], Option[PseudoElement], Set[PseudoClass])
				type Accumulator = Try[Components]
				val initialValue: Accumulator = Success((None, None, Set.empty[SimpleSelector], None, Set.empty[PseudoClass]))

				val result: Accumulator = selectors.foldLeft[Accumulator](initialValue)(
					(accumulator: Accumulator, selector: NormalizedSelector) => accumulator match {
						case Failure(exception) => Failure(exception)
						case Success((element, id, subClasses, pseudoElement, furtherPseudoClasses)) => selector match {
							case simple: SimpleSelector => simple match {
								case Element(anotherElement) => element match { // TODO check everything else is empty
									case Some(_) => Failure(SelectorError(MultipleTypeSelectors))
									case None => pseudoElement match {
										case Some(_) => Failure(SelectorError(IllegalSelectorAfterPseudoElement))
										case None =>
											/*anotherElement.element match { TODO check the env for custom element name
												case CustomElement(name) =>
												case _ =>
											}*/
											Success(Some(Element(anotherElement)), id, subClasses, pseudoElement, furtherPseudoClasses)
									}
								}
								case PseudoElement(anotherPseudoElement) => pseudoElement match {
									case Some(_) => Failure(SelectorError(MultiplePseudoElements))
									case None =>
										/*anotherPseudoElement match { TODO check the env for custom pseudo element name
											case CustomPseudoElement(name) =>
											case _ =>
										}*/
										Success(element, id, subClasses, Some(PseudoElement(anotherPseudoElement)), furtherPseudoClasses)
								}
								case anotherId: Id => id match {
									case Some(_) => Failure(SelectorError(MultipleIdSelectors))
									case None => Success(element, Some(anotherId), subClasses, pseudoElement, furtherPseudoClasses)
								}
								case Attribute(_, _, _) | Class(_) =>
									Success(element, id, subClasses + simple, pseudoElement, furtherPseudoClasses)
								case pseudo: PseudoClass => pseudoElement match {
									case Some(_) => Success(element, id, subClasses, pseudoElement, furtherPseudoClasses + pseudo)
									case None => Success(element, id, subClasses + pseudo, pseudoElement, furtherPseudoClasses)
								}
							}
							case _ =>
								// This REALLY shouldn't happen
								Failure(CompilerError("Non-simple selector within a RawCompound selector", rawCompound))
						}
					}
				)

				result match {
					case Failure(exception) => Failure(exception)
					case Success((element, id, subClasses, pseudoElement, furtherPseudoClasses)) =>
						val allSubClasses = id match {
							case Some(idSelector) => subClasses + idSelector
							case None => subClasses
						}
						Success(Compound(element, allSubClasses, pseudoElement, furtherPseudoClasses))
				}
		}
	}


	// TODO validate illegal combinator use
	private def normalizeRawComplex(rawComplex: RawComplex)(implicit environment: Environment): Try[Complex] =
		chainNormalize(rawComplex.left :: rawComplex.right :: Nil) match {
			case Failure(exception) => Failure(exception)
			case Success(normalizedLeft :: normalizedRight :: Nil) =>
				Success(Complex(rawComplex.combinator, normalizedLeft, normalizedRight))
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
					Success(SelectorList(normalizedSet))
		}


	private def chainNormalize(selectors: Seq[Selector])(implicit environment: Environment): Try[Seq[NormalizedSelector]] =
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

}
