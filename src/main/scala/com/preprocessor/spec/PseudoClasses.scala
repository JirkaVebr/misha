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


	/**
		* @see https://drafts.csswg.org/selectors-4/#drag-pseudos
		*/
	sealed trait DropFilter extends ArgumentEnum

	case object Active extends DropFilter {
		override def value: String = "active"
	}
	case object Valid extends DropFilter {
		override def value: String = "valid"
	}
	case object Invalid extends DropFilter {
		override def value: String = "invalid"
	}


	/**
		* @see https://drafts.csswg.org/selectors-4/#child-index
		*/
	sealed trait Nth extends PseudoClass {
		def kind: String

		override def name: String = "nth-" + kind
	}

	case object Child extends Nth {
		override def kind: String = "child"
	}
	case object LastChild extends Nth {
		override def kind: String = "last-child"
	}
	case object OfType extends Nth {
		override def kind: String = "of-type"
	}
	case object LastOfType extends Nth {
		override def kind: String = "last-of-type"
	}
	case object Col extends Nth {
		override def kind: String = "col"
	}
	case object LastCol extends Nth {
		override def kind: String = "last-col"
	}

	/**
		* No need for special cases for `even` and `odd` as these reduce to 2n and 2n+1 respectively
		* @see https://drafts.csswg.org/css-syntax-3/#anb-microsyntax
		*/
	case class AnPlusB(a: Int, b: Int)

}
