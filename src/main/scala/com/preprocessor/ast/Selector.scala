package com.preprocessor.ast

import com.preprocessor.spec.AttributeSelector.Modifier
import com.preprocessor.spec.PseudoClasses.NonFunctional.NonFunctionalPseudoClass
import com.preprocessor.spec.PseudoClasses.{AnPlusB, Directionality, DropFilter}
import com.preprocessor.spec.{PseudoClasses, PseudoElements}


object Selector {
	sealed trait Selector

	sealed trait SimpleSelector extends Selector
	case class Elemental(element: QualifiedElement) extends SimpleSelector
	case class Attribute(name: QualifiedAttribute, target: Option[MatchTarget] = None, modifier: Option[Modifier] = None)
		extends SimpleSelector // target being None signifies checking just for attribute presence
	case class Id(name: CssIdentifier) extends SimpleSelector
	case class Class(name: CssIdentifier) extends SimpleSelector

	sealed trait PseudoClass extends SimpleSelector
	case class SubSelector(kind: SubSelector, subSelectors: Set[Selector]) extends PseudoClass
	case class Dir(directionality: Directionality) extends PseudoClass
	case class Drop(filter: Option[DropFilter]) extends PseudoClass
	case class Lang(name: CssIdentifier) extends PseudoClass
	case class NonFunctional(pseudoClass: NonFunctionalPseudoClass) extends PseudoClass
	case class CustomPseudoClass(name: CssIdentifier) extends PseudoClass
	case class Nth(kind: PseudoClasses.Nth, ab: AnPlusB, of: Option[Selector]) extends PseudoClass

	sealed trait PseudoElement extends SimpleSelector
	case class SpecifiedPseudoElement(element: PseudoElements.PseudoElement) extends PseudoElement
	case class CustomPseudoElement(name: CssIdentifier) extends PseudoElement

	case class Compound(selectors: Set[SimpleSelector]) extends Selector

	case class Complex(head: Compound, tail: Seq[ComplexSelectorComponent]) extends Selector

	case class SelectorList(selectors: Set[Selector]) extends Selector

}
