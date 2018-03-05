package com.preprocessor.spec

object PseudoClasses {

	sealed trait PseudoClass {
		def name: String
	}

	sealed trait SubSelector extends PseudoClass

	case object Not extends SubSelector {
		override def name: String = "not"
	}
	case object Matches extends SubSelector {
		override def name: String = "matches"
	}
	case object Something extends SubSelector {
		override def name: String = "something"
	}
	case object Has extends SubSelector {
		override def name: String = "has"
	}
	case object Current extends SubSelector {
		override def name: String = "current"
	}

	val subSelectors: Map[String, SubSelector] = Map(
		Not.name -> Not,
		Matches.name -> Matches,
		Something.name -> Something,
		Has.name -> Has,
		Current.name -> Current
	)


	trait ArgumentEnum {
		def value: String
	}


	/**
		* @see https://drafts.csswg.org/selectors-4/#the-dir-pseudo
		*/
	sealed trait Directionality extends ArgumentEnum

	case object Ltr extends Directionality {
		override def value: String = "ltr"
	}
	case object Rtl extends Directionality {
		override def value: String = "rtl"
	}
	case class UndefinedDirectionality(value: String) extends Directionality

	val directionality: Map[String, Directionality] = Map(
		Ltr.value -> Ltr,
		Rtl.value -> Rtl
	)

}
