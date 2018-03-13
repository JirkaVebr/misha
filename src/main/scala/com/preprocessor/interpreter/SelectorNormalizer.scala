package com.preprocessor.interpreter

import com.preprocessor.ast.Selector._

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
			case RawCompound(selectors) => sys.error("todo")
			case RawComplex(combinator, left, right) => chainNormalize(left :: right :: Nil) match {
				case Failure(exception) => Failure(exception)
				case Success(normalizedLeft :: normalizedRight :: Nil) =>
					Success(Complex(combinator, normalizedLeft, normalizedRight))
				case _ => sys.error("this shouldn't happen") // TODO
			}
			case RawSelectorList(selectors) => chainNormalize(selectors) match {
				case Failure(exception) => Failure(exception)
				case Success(normalizedSelectors) => Success(SelectorList(normalizedSelectors.toSet))
			}
		}
		case normalized: NormalizedSelector => Success(normalized)
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
