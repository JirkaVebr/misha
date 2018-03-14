package com.preprocessor.interpreter.validators

import com.preprocessor.ast.Selector._
import com.preprocessor.error.{CompilerError, SelectorError}
import com.preprocessor.error.SelectorError.{IllegalSelectorAfterPseudoElement, MultipleIdSelectors, MultiplePseudoElements, MultipleTypeSelectors}
import com.preprocessor.interpreter.Environment
import com.preprocessor.interpreter.validators.SelectorNormalizer.chainNormalize

import scala.util.{Failure, Success, Try}

object RawCompoundNormalizer {

	def normalize(rawCompound: RawCompound)(implicit environment: Environment): Try[Compound] = {
		chainNormalize(rawCompound.selectors) match {
			case Failure(exception) => Failure(exception)
			case Success(selectors) => // Assuming there are at least two selectors
				type Components = (Option[Element], Option[Id], Set[SimpleSelector], Option[PseudoElement], Set[PseudoClass])
				type Accumulator = Try[Components]
				val initialValue: Accumulator = Success((None, None, Set.empty[SimpleSelector], None, Set.empty[PseudoClass]))

				val result: Accumulator = selectors.foldLeft[Accumulator](initialValue)(
					(accumulator: Accumulator, selector: NormalizedSelector) => accumulator match {
						case Failure(exception) =>
							Failure(exception)
						case Success((element, id, subClasses, pseudoElement, furtherPseudoClasses)) => selector match {
							case simple: SimpleSelector => simple match {
								case Element(anotherElement) => element match { // TODO check everything else is empty
									case Some(_) =>
										Failure(SelectorError(MultipleTypeSelectors))
									case None => pseudoElement match {
										case Some(_) =>
											Failure(SelectorError(IllegalSelectorAfterPseudoElement))
										case None =>
											/*anotherElement.element match { TODO check the env for custom element name
												case CustomElement(name) =>
												case _ =>
											}*/
											Success(Some(Element(anotherElement)), id, subClasses, pseudoElement, furtherPseudoClasses)
									}
								}
								case PseudoElement(anotherPseudoElement) => pseudoElement match {
									case Some(_) =>
										Failure(SelectorError(MultiplePseudoElements))
									case None =>
										/*anotherPseudoElement match { TODO check the env for custom pseudo element name
											case CustomPseudoElement(name) =>
											case _ =>
										}*/
										Success(element, id, subClasses, Some(PseudoElement(anotherPseudoElement)), furtherPseudoClasses)
								}
								case anotherId: Id => id match {
									case Some(_) =>
										Failure(SelectorError(MultipleIdSelectors))
									case None =>
										Success(element, Some(anotherId), subClasses, pseudoElement, furtherPseudoClasses)
								}
								case Attribute(_, _, _) | Class(_) =>
									Success(element, id, subClasses + simple, pseudoElement, furtherPseudoClasses)
								case pseudo: PseudoClass => pseudoElement match {
									case Some(_) =>
										Success(element, id, subClasses, pseudoElement, furtherPseudoClasses + pseudo)
									case None =>
										Success(element, id, subClasses + pseudo, pseudoElement, furtherPseudoClasses)
								}
							}
							case _ =>
								// This REALLY shouldn't happen
								Failure(CompilerError("Non-simple selector within a RawCompound selector", rawCompound))
						}
					}
				)

				result match {
					case Failure(exception) =>
						Failure(exception)
					case Success((element, id, subClasses, pseudoElement, furtherPseudoClasses)) =>
						val allSubClasses = id match {
							case Some(idSelector) => subClasses + idSelector
							case None => subClasses
						}
						Success(Compound(element, allSubClasses, pseudoElement, furtherPseudoClasses))
				}
		}
	}

}
