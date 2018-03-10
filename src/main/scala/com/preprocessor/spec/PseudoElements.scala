package com.preprocessor.spec

object PseudoElements {

	/**
		* @see https://drafts.csswg.org/css-pseudo-4/
		*/
	sealed trait PseudoElement {
		val name: String
	}

	case object After extends PseudoElement { override val name: String = "after"}
	case object Before extends PseudoElement { override val name: String = "before"}
	case object FirstLetter extends PseudoElement { override val name: String = "first-letter"}
	case object FirstLine extends PseudoElement { override val name: String = "first-line"}
	case object GrammarError extends PseudoElement { override val name: String = "grammar-error"}
	case object InactiveSelection extends PseudoElement { override val name: String = "inactive-selection"}
	case object Marker extends PseudoElement { override val name: String = "marker"}
	case object Placeholder extends PseudoElement { override val name: String = "placeholder"}
	case object Selection extends PseudoElement { override val name: String = "selection"}
	case object SpellingError extends PseudoElement { override val name: String = "spelling-error"}

	case class CustomPseudoElement(name: String) extends PseudoElement


	val pseudoElements: Map[String, PseudoElement] = Map(
		After.name -> After,
		Before.name -> Before,
		FirstLetter.name -> FirstLetter,
		FirstLine.name -> FirstLine,
		GrammarError.name -> GrammarError,
		InactiveSelection.name -> InactiveSelection,
		Marker.name -> Marker,
		Placeholder.name -> Placeholder,
		Selection.name -> Selection,
		SpellingError.name -> SpellingError
	)

}
