package com.preprocessor.ast

import com.preprocessor.ast.Ast.Term.MagicSymbol
import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Symbol.ValueSymbol

object RuleContext {

	sealed trait RuleContext

	case class RawRuleHead(components: List[Either[String, MagicSymbol]]) extends RuleContext

	object Selector {
		sealed trait Selector extends RuleContext

		case object Universal extends Selector
		case class Element(name: String, namespace: Option[Namespace.Namespace] = None) extends Selector
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


	object Keyframes {
		case class Keyframes(identifier: ValueSymbol) extends RuleContext

		sealed trait Keyframe extends RuleContext

		case object From extends Keyframe
		case object To extends Keyframe
		case class Percentage(value: Value.Percentage) extends Keyframe
	}

}
