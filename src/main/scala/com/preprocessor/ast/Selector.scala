package com.preprocessor.ast

object Selector {

	sealed trait Selector

	case object Universal extends Selector
	case class Element(name: String, namespace: Option[Namespace] = None) extends Selector
	case class Class(name: String) extends Selector
	case class Id(name: String) extends Selector


	case class Binary(combinator: Combinator, left: Selector, right: Selector) extends Selector
	sealed trait Combinator

	case object Multiple extends Combinator // A, B
	case object Descendant extends Combinator // A B
	case object Child extends Combinator // A > B
	case object AdjacentSibling extends Combinator // A + B
	case object GeneralSibling extends Combinator // A ~ B


	case class Attribute(matcher: Matcher, name: String, value: Option[String] = None, modifier: Option[Modifier] = None)

	sealed trait Modifier
	case object CaseInsensitive extends Modifier

	sealed trait Matcher
	case object Equals extends Matcher // [attr=value]
	case object Includes extends Matcher // [attr~=value]
	case object Prefix extends Matcher // [attr|=value]
	case object Preceded extends Matcher // [attr^=value]
	case object Followed extends Matcher // [attr$=value]
	case object Contains extends Matcher // [attr*=value]



}
