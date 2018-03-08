package com.preprocessor.spec

object PseudoElements {

	/**
		* @see https://drafts.csswg.org/css-pseudo-4/
		*/
	sealed trait PseudoElement {
		def name: String
	}

	case object After extends PseudoElement { override def name: String = "after"}
	case object Before extends PseudoElement { override def name: String = "before"}
	case object FirstLetter extends PseudoElement { override def name: String = "first-letter"}
	case object FirstLine extends PseudoElement { override def name: String = "first-line"}
	case object GrammarError extends PseudoElement { override def name: String = "grammar-error"}
	case object InactiveSelection extends PseudoElement { override def name: String = "inactive-selection"}
	case object Marker extends PseudoElement { override def name: String = "marker"}
	case object Placeholder extends PseudoElement { override def name: String = "placeholder"}
	case object Selection extends PseudoElement { override def name: String = "selection"}
	case object SpellingError extends PseudoElement { override def name: String = "spelling-error"}

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
