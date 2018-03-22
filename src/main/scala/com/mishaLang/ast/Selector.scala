package com.mishaLang.ast

import com.mishaLang.emitter.SelectorEmitter
import com.mishaLang.spec.AttributeSelector.Modifier
import com.mishaLang.spec.PseudoClasses.NonFunctional.NonFunctionalPseudoClass
import com.mishaLang.spec.PseudoClasses.{AnPlusB, Directionality, DropFilter}
import com.mishaLang.spec.SelectorSeparator.Combinator
import com.mishaLang.spec.{PseudoClasses, PseudoElements}


object Selector {
	sealed trait Selector

	/** Raw selectors exist because they can contain silly selectors such as "div#foo#bar.baz.baz::before span" */
	sealed trait RawSelector extends Selector
	sealed trait NormalizedSelector extends Selector {
		override lazy val toString: String = SelectorEmitter.emit(this)(StringBuilder.newBuilder).mkString
	}

	sealed trait ComplexComponent extends NormalizedSelector
	sealed trait CompoundComponent extends ComplexComponent

	sealed trait RawComplexComponent extends RawSelector
	sealed trait RawCompoundComponent extends RawComplexComponent


	sealed trait SimpleSelector extends CompoundComponent
	sealed trait RawSimpleSelector extends RawCompoundComponent

	/** @see https://drafts.csswg.org/selectors-4/#typedef-subclass-selector */
	sealed trait SubClass extends SimpleSelector
	sealed trait RawSubClass extends RawSimpleSelector

	case class Element(element: QualifiedElement) extends SimpleSelector with RawSimpleSelector
	case class PseudoElement(element: PseudoElements.PseudoElement) extends SimpleSelector with RawSimpleSelector
	case class Attribute(name: QualifiedAttribute, target: Option[MatchTarget] = None, modifier: Option[Modifier] = None)
		extends SubClass with RawSubClass // target being None signifies checking just for attribute presence
	case class Id(name: CssIdentifier) extends SubClass with RawSubClass
	case class Class(name: CssIdentifier) extends SubClass with RawSubClass

	sealed trait PseudoClass extends SubClass
	sealed trait RawPseudoClass extends RawSubClass

	case class NonFunctional(pseudoClass: NonFunctionalPseudoClass) extends PseudoClass with RawPseudoClass
	case class Dir(directionality: Directionality) extends PseudoClass with RawPseudoClass
	case class Drop(filter: Set[DropFilter]) extends PseudoClass with RawPseudoClass
	case class Lang(ranges: Set[CssIdentifier]) extends PseudoClass with RawPseudoClass // TODO add this to spec

	case class SubSelector(kind: PseudoClasses.SubSelector, subSelector: NormalizedSelector) extends PseudoClass
	case class RawSubSelector(kind: PseudoClasses.SubSelector, subSelector: Selector) extends RawPseudoClass

	case class Nth(kind: PseudoClasses.Nth, ab: AnPlusB, of: Option[NormalizedSelector] = None) extends PseudoClass
	case class RawNth(kind: PseudoClasses.Nth, ab: AnPlusB, of: Option[Selector] = None) extends RawPseudoClass


	/**
		* According to the spec, there can be up to one type-element (@see https://drafts.csswg.org/selectors-4/#compound).
		* According to the current spec, there can be up to one pseudo-element (@see https://www.w3.org/TR/selectors-3/#pseudo-elements).
		* However, it also says that a future spec may allow for more, which si what the Level 4 grammar does
		* (@see https://drafts.csswg.org/selectors-4/#typedef-compound-selector). Nevertheless, it doesn't specify more,
		* and so we're just disregarding that here.
		*/
	case class Compound(element: Option[Element], subClassSelectors: Set[SubClass],
											pseudoElement: Option[PseudoElement], furtherPseudoClasses: Set[PseudoClass]) extends CompoundComponent
	case class RawCompound(selectors: Seq[RawSimpleSelector]) extends RawCompoundComponent

	case class Complex(combinator: Combinator, left: ComplexComponent, right: ComplexComponent) extends ComplexComponent
	case class RawComplex(combinator: Combinator, left: RawComplexComponent, right: RawComplexComponent) extends RawComplexComponent

	case class SelectorList(selectors: Set[ComplexComponent]) extends NormalizedSelector
	case class RawSelectorList(selectors: Seq[RawComplexComponent]) extends RawSelector

}
