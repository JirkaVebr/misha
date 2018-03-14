package com.preprocessor.interpreter.validators

import com.preprocessor.ast.Selector._
import com.preprocessor.error.SelectorError
import com.preprocessor.error.SelectorError._
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
				RawCompoundNormalizer.normalize(rawCompound)
			case rawComplex: RawComplex =>
				normalizeRawComplex(rawComplex)
			case rawSelectorList: RawSelectorList =>
				normalizeSelectorList(rawSelectorList)
		}
		case normalized: NormalizedSelector => Success(normalized) // TODO validate
	}


	// TODO validate illegal combinator use
	private def normalizeRawComplex(rawComplex: RawComplex)(implicit environment: Environment): Try[Complex] =
		chainNormalize(rawComplex.left :: rawComplex.right :: Nil) match {
			case Failure(exception) =>
				Failure(exception)
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

}
