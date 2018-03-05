package com.preprocessor.spec

object SelectorCombinator {

	sealed trait Combinator

	case object Combine extends Combinator // AB
	case object Multiple extends Combinator // A, B
	case object Descendant extends Combinator // A B
	case object Child extends Combinator // A > B
	case object NextSibling extends Combinator // A + B
	case object SubsequentSibling extends Combinator // A ~ B
}