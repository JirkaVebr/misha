package com.preprocessor.ast

import com.preprocessor.emitter.SelectorEmitter
import com.preprocessor.spec.AttributeSelector.Modifier
import com.preprocessor.spec.PseudoClasses.NonFunctional.NonFunctionalPseudoClass
import com.preprocessor.spec.PseudoClasses.{AnPlusB, Directionality, DropFilter}
import com.preprocessor.spec.SelectorCombinator.Combinator
import com.preprocessor.spec.{PseudoClasses, PseudoElements}


object Selector {
	sealed trait Selector

	/**
		* Raw selectors exist because they can contain silly selectors such as "div#foo#bar.baz.baz::before span"
		*/
	sealed trait RawSelector extends Selector
	sealed trait NormalizedSelector extends Selector {
		override lazy val toString: String = SelectorEmitter.emit(this)(StringBuilder.newBuilder).mkString
	}


	sealed trait SimpleSelector extends NormalizedSelector

	case class Element(element: QualifiedElement) extends SimpleSelector
	case class PseudoElement(element: PseudoElements.PseudoElement) extends SimpleSelector
	case class Attribute(name: QualifiedAttribute, target: Option[MatchTarget] = None, modifier: Option[Modifier] = None)
		extends SimpleSelector // target being None signifies checking just for attribute presence
	case class Id(name: CssIdentifier) extends SimpleSelector
	case class Class(name: CssIdentifier) extends SimpleSelector

	sealed trait PseudoClass extends SimpleSelector

	case class NonFunctional(pseudoClass: NonFunctionalPseudoClass) extends PseudoClass
	case class Dir(directionality: Directionality) extends PseudoClass
	case class Drop(filter: Set[DropFilter]) extends PseudoClass
	case class Lang(ranges: Set[CssIdentifier]) extends PseudoClass // TODO add this to spec

	case class SubSelector(kind: PseudoClasses.SubSelector, subSelector: NormalizedSelector) extends PseudoClass
	case class RawSubSelector(kind: PseudoClasses.SubSelector, subSelector: Selector) extends RawSelector

	case class Nth(kind: PseudoClasses.Nth, ab: AnPlusB, of: Option[NormalizedSelector] = None) extends PseudoClass
	case class RawNth(kind: PseudoClasses.Nth, ab: AnPlusB, of: Option[Selector] = None) extends RawSelector


	/**
		* According to the spec, there can be up to one type-element (@see https://drafts.csswg.org/selectors-4/#compound).
		* According to the current spec, there can be up to one pseudo-element (@see https://www.w3.org/TR/selectors-3/#pseudo-elements).
		* However, it also says that a future spec may allow for more, which si what the Level 4 grammar does
		* (@see https://drafts.csswg.org/selectors-4/#typedef-compound-selector). Nevertheless, it doesn't specify more,
		* and so we're just disregarding that here.
		*/
	case class Compound(element: Option[Element], subClassSelectors: Set[SimpleSelector],
											pseudoElement: Option[PseudoElement], furtherPseudoClasses: Set[PseudoClass]) extends NormalizedSelector
	case class RawCompound(selectors: Seq[Selector]) extends RawSelector

	case class Complex(combinator: Combinator, left: NormalizedSelector, right: NormalizedSelector) extends NormalizedSelector
	case class RawComplex(combinator: Combinator, left: Selector, right: Selector) extends RawSelector

	case class SelectorList(selectors: Set[NormalizedSelector]) extends NormalizedSelector
	case class RawSelectorList(selectors: Seq[Selector]) extends RawSelector

}
