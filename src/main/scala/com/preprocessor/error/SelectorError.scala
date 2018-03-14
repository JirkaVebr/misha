package com.preprocessor.error

import com.preprocessor.ast.Selector.Selector
import com.preprocessor.error.SelectorError.SelectorErrorCode

class SelectorError(val errorCode: SelectorErrorCode, selectors: Selector*) extends Error


object SelectorError {
	sealed trait SelectorErrorCode {
		def message(selectors: Selector*): String
	}

	abstract class SimpleError(val message: String) extends SelectorErrorCode {
		override def message(selectors: Selector*): String = message
	}

	def apply(errorCode: SelectorErrorCode, selectors: Selector*): SelectorError =
		new SelectorError(errorCode, selectors: _*)

	case object DuplicateSelectorInList extends SimpleError("Duplicate selector within a selector list")
	case object IllegalSelectorAfterPseudoElement extends SimpleError("Illegal selector after a pseudo-element: only pseudo-classes can appear there")
	case object MultipleIdSelectors extends SimpleError("Multiple id selectors within a single compound selector")
	case object MultiplePseudoElements extends SimpleError("Multiple pseudo elements within a single compound selector")
	case object MultipleTypeSelectors extends SimpleError("Multiple type selectors within a single compound selector")
}
