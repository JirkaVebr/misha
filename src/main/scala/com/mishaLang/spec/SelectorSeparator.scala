package com.mishaLang.spec

object SelectorSeparator {

	sealed trait SelectorSeparator {
		def symbol: String
		def emit: String = " " + symbol + " "
	}

	sealed trait Combinator extends SelectorSeparator

	case object Descendant extends Combinator {
		override def symbol: String = " "
		override def emit: String = symbol
	}
	case object Child extends Combinator {
		override def symbol: String = ">"
	}
	case object NextSibling extends Combinator {
		override def symbol: String = "+"
	}
	case object SubsequentSibling extends Combinator {
		override def symbol: String = "~"
	}


	case object SelectorListSeparator extends SelectorSeparator {
		override def symbol: String = ","
	}
}
