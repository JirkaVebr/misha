package com.preprocessor.ast

import com.preprocessor.ast.RuleContext.RuleContext
import com.preprocessor.spec.AttributeSelector.Modifier
import com.preprocessor.spec.PseudoClasses
import com.preprocessor.spec.PseudoClasses.NonFunctional.NonFunctionalPseudoClass
import com.preprocessor.spec.PseudoClasses.{AnPlusB, Directionality, DropFilter}
import com.preprocessor.spec.PseudoElements
import com.preprocessor.spec.SelectorCombinator.Combinator


object Selector {
	sealed trait Selector

	case object Universal extends Selector
	case class Compound(combinator: Combinator, left: Selector, right: Selector) extends Selector

	case class Element(name: CssIdentifier, namespace: Option[Namespace.Namespace] = None) extends Selector
	case class Class(name: CssIdentifier) extends Selector
	case class Id(name: CssIdentifier) extends Selector

	sealed trait PseudoClass extends Selector
	case class SubSelector(kind: SubSelector, subSelectors: Seq[Selector]) extends PseudoClass
	case class Dir(directionality: Directionality) extends PseudoClass
	case class Drop(filter: Option[DropFilter]) extends PseudoClass
	case class Lang(name: CssIdentifier) extends PseudoClass
	case class NonFunctional(pseudoClass: NonFunctionalPseudoClass) extends PseudoClass
	case class CustomPseudoClass(name: CssIdentifier) extends PseudoClass
	case class Nth(kind: PseudoClasses.Nth, ab: AnPlusB, of: Option[Selector]) extends PseudoClass

	sealed trait PseudoElement extends Selector
	case class SpecifiedPseudoElement(element: PseudoElements.PseudoElement) extends PseudoElement
	case class CustomPseudoElement(name: CssIdentifier) extends PseudoElement

	// target being None signifies checking just for attribute presence
	case class Attribute(name: String, target: Option[MatchTarget] = None, modifier: Option[Modifier] = None)

}
