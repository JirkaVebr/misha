package com.preprocessor.spec

object SelectorCombinator {

	sealed trait Combinator {
		def symbol: String
		def emit: String = " " + symbol + " "
	}

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
}
