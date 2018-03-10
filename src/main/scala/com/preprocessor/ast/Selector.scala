package com.preprocessor.ast

import com.preprocessor.spec.AttributeSelector.Modifier
import com.preprocessor.spec.PseudoClasses.NonFunctional.NonFunctionalPseudoClass
import com.preprocessor.spec.PseudoClasses.{AnPlusB, Directionality, DropFilter}
import com.preprocessor.spec.{PseudoClasses, PseudoElements}


object Selector {
	sealed trait Selector

	/** Raw selectors exist because they can contain silly selectors such as "div#foo#bar.baz.baz::before span"
		* Non-raw selectors should be valid though
		*/
	sealed trait RawSelector extends Selector

	sealed trait SimpleSelector extends Selector
	case class Element(element: QualifiedElement) extends SimpleSelector
	case class PseudoElement(element: PseudoElements.PseudoElement) extends SimpleSelector
	case class Attribute(name: QualifiedAttribute, target: Option[MatchTarget] = None, modifier: Option[Modifier] = None)
		extends SimpleSelector // target being None signifies checking just for attribute presence
	case class Id(name: CssIdentifier) extends SimpleSelector
	case class Class(name: CssIdentifier) extends SimpleSelector

	sealed trait PseudoClass extends SimpleSelector
	case class SubSelector(kind: PseudoClasses.SubSelector, subSelector: Selector) extends PseudoClass
	case class RawSubSelector(kind: PseudoClasses.SubSelector, subSelector: Selector) extends PseudoClass with RawSelector
	case class Dir(directionality: Directionality) extends PseudoClass
	case class Drop(filter: Set[DropFilter]) extends PseudoClass
	case class Lang(name: CssIdentifier) extends PseudoClass
	case class Nth(kind: PseudoClasses.Nth, ab: AnPlusB, of: Option[Selector] = None) extends PseudoClass
	case class RawNth(kind: PseudoClasses.Nth, ab: AnPlusB, of: Option[Selector] = None) extends PseudoClass with RawSelector
	case class NonFunctional(pseudoClass: NonFunctionalPseudoClass) extends PseudoClass

	case class Compound(selectors: Set[SimpleSelector]) extends Selector
	case class RawCompound(selectors: Seq[SimpleSelector]) extends RawSelector

	case class Complex(head: Compound, tail: Seq[ComplexSelectorComponent]) extends Selector
	case class RawComplex(head: Compound, tail: Seq[ComplexSelectorComponent]) extends RawSelector

	case class SelectorList(selectors: Set[Selector]) extends Selector
	case class RawSelectorList(selectors: Seq[Selector]) extends RawSelector

}
