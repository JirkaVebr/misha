package com.preprocessor.interpreter

import com.preprocessor.ast.Selector.{NormalizedSelector, RawSelector}
import com.preprocessor.interpreter.validators.SelectorNormalizer

import scala.util.{Failure, Success, Try}

object SelectorValidator {

	def validateSelector(selector: RawSelector)(implicit state: EnvWithValue): Try[NormalizedSelector] = {
		SelectorNormalizer.normalize(selector) match {
			case Failure(exception) => Failure(exception)
			case Success(normalizedSelector) => validateSelector(normalizedSelector)
		}
	}

	def validateSelector(selector: NormalizedSelector)(implicit state: EnvWithValue): Try[NormalizedSelector] =
		Success(selector) // TODO

}
