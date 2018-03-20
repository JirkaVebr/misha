package com.preprocessor.interpreter.validators

import com.preprocessor.ast.Selector._
import com.preprocessor.error.{CompilerError, SelectorError}
import com.preprocessor.error.SelectorError.{IllegalSelectorAfterPseudoElement, MultipleIdSelectors, MultiplePseudoElements, MultipleTypeSelectors}
import com.preprocessor.interpreter.Environment
import com.preprocessor.interpreter.validators.SelectorNormalizer.chainNormalize

import scala.util.{Failure, Success, Try}

object RawCompoundNormalizer {

	private case class Components(element: Option[Element], id: Option[Id], subClasses: Set[SubClass],
																pseudoElement: Option[PseudoElement], furtherPseudoClasses: Set[PseudoClass]) {

		def withNewElement(element: Element): Success[Components] =
			withNew(Components(Some(element), id, subClasses, pseudoElement, furtherPseudoClasses))

		def withNewId(id: Id): Success[Components] =
			withNew(Components(element, Some(id), subClasses, pseudoElement, furtherPseudoClasses))

		def withNewSubClass(subClass: SubClass): Success[Components] =
			withNew(Components(element, id, subClasses + subClass, pseudoElement, furtherPseudoClasses))

		def withNewPseudoElement(pseudoElement: PseudoElement): Success[Components] =
			withNew(Components(element, id, subClasses, Some(pseudoElement), furtherPseudoClasses))

		def withNewFurtherPseudoClass(pseudoClass: PseudoClass): Success[Components] =
			withNew(Components(element, id, subClasses, pseudoElement, furtherPseudoClasses + pseudoClass))

		private def withNew(components: Components): Success[Components] =
			Success(components)

	}

	private type Accumulator = Try[Components]


	def normalize(rawCompound: RawCompound)(implicit environment: Environment): Try[Compound] = {
		chainNormalize(rawCompound.selectors) match {
			case Failure(exception) => Failure(exception)
			case Success(selectors) => // Assuming there are at least two selectors
				val initialValue: Accumulator =
					Success(Components(None, None, Set.empty[SubClass], None, Set.empty[PseudoClass]))

				val result: Accumulator = selectors.foldLeft[Accumulator](initialValue)(
					(accumulator: Accumulator, selector: NormalizedSelector) => accumulator match {
						case Failure(exception) =>
							Failure(exception)
						case Success(components) => selector match {
							case simple: SimpleSelector => simple match {
								case anotherElement: Element =>
									normalizeElement(anotherElement, components)
								case pseudoElement: PseudoElement =>
									normalizePseudoElement(pseudoElement, components)
								case anotherId: Id => components.id match {
									case Some(_) =>
										Failure(SelectorError(MultipleIdSelectors))
									case None =>
										components.withNewId(anotherId)
								}
								case attribute: Attribute =>
									components.withNewSubClass(attribute)
								case classSelector: Class =>
									components.withNewSubClass(classSelector)
								case pseudo: PseudoClass => components.pseudoElement match {
									case Some(_) =>
										components.withNewFurtherPseudoClass(pseudo)
									case None =>
										components.withNewSubClass(pseudo)
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
					case Success(Components(element, id, subClasses, pseudoElement, furtherPseudoClasses)) =>
						val allSubClasses = id match {
							case Some(idSelector) => subClasses + idSelector
							case None => subClasses
						}
						Success(Compound(element, allSubClasses, pseudoElement, furtherPseudoClasses))
				}
		}
	}


	private def normalizeElement(element: Element, components: Components)(implicit environment: Environment): Accumulator =
		components.element match { // TODO check everything else is empty
			case Some(_) =>
				Failure(SelectorError(MultipleTypeSelectors))
			case None => components.pseudoElement match {
				case Some(_) =>
					Failure(SelectorError(IllegalSelectorAfterPseudoElement))
				case None =>
					/*anotherElement.element match { TODO check the env for custom element name
						case CustomElement(name) =>
						case _ =>
					}*/
					components.withNewElement(element)
			}
		}


	private def normalizePseudoElement(pseudoElement: PseudoElement, components: Components)(implicit environment: Environment): Accumulator =
		components.pseudoElement match {
			case Some(_) =>
				Failure(SelectorError(MultiplePseudoElements))
			case None =>
				/*anotherPseudoElement match { TODO check the env for custom pseudo element name
					case CustomPseudoElement(name) =>
					case _ =>
				}*/
				components.withNewPseudoElement(pseudoElement)
		}


}
