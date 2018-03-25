package com.mishaLang.parser.ruleHead

import com.mishaLang.ast
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Selector._
import com.mishaLang.ast.{CssIdentifier, MatchTarget}
import com.mishaLang.parser.common.{L0_Whitespace, L1_AstNode, L2_Strings, L3_Numbers}
import com.mishaLang.spec.AttributeSelector.{Matcher, Modifier}
import com.mishaLang.spec.PseudoClasses.NonFunctional.CustomPseudoClass
import com.mishaLang.spec.PseudoClasses.UndefinedDirectionality
import com.mishaLang.spec.PseudoElements.CustomPseudoElement
import com.mishaLang.spec.SelectorSeparator._
import com.mishaLang.spec.{AttributeSelector, PseudoClasses, PseudoElements}
import org.parboiled2.{StringBuilding, _}
import shapeless.{::, HNil}

import scala.annotation.switch

/**
	* TODO special char escaping
	* @see https://drafts.csswg.org/selectors-4/#case-sensitive
	*/
trait L6_Selector { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_AstNode
	with L2_Strings
	with L3_Numbers
	with L4_Basics
	with L5_AnPlusB =>

	def Selector: Rule1[RawSelector] = rule {
		selectorList
	}

	private def selectorList: Rule1[RawSelector] = rule {
		oneOrMore(complexSelector).separatedBy(AnyWhitespaceAround(",")) ~> (
			(selectors: Seq[RawComplexComponent]) =>
				if (selectors.lengthCompare(1) == 0) selectors.head
				else RawSelectorList(selectors)
		)
	}

	private def complexSelector: Rule1[RawComplexComponent] = rule { // Right associative
		compoundSelector ~ zeroOrMore(combinator ~ complexSelector ~> (
			(left: RawComplexComponent, combinator: Combinator, right: RawComplexComponent) => RawComplex(combinator, left, right)
		))
	}

	private def combinator: Rule1[Combinator] = rule {
		(WhitespaceAround(">") ~ push(Child)) |
		(WhitespaceAround("+") ~ push(NextSibling)) |
		(WhitespaceAround("~") ~ push(SubsequentSibling)) |
		(MandatorySingleLineWhitespace ~ push(Descendant))
	}

	/**
		* @see https://drafts.csswg.org/selectors-4/#typedef-compound-selector
		*/
	private def compoundSelector: Rule1[RawCompoundComponent] = rule {
		oneOrMore(simpleSelector) ~> (
			(selectors: Seq[RawSimpleSelector]) =>
				if (selectors.lengthCompare(1) == 0) selectors.head
				else RawCompound(selectors)
		)
	}

	private def simpleSelector: Rule1[RawSimpleSelector] = rule {
		run {
			(cursorChar: @switch) match {
				case '#' => id
				case '.' => actualClass
				case ':' => pseudo
				case '[' => attribute
				case _ => element
			}
		}
	}

	private def id: Rule1[Id] = rule {
		'#' ~!~ CssIdentifier ~> Id
	}

	private def actualClass: Rule1[Class] = rule {
		'.' ~!~ CssIdentifier ~> Class
	}

	private def element: Rule1[Element] = rule {
		QualifiedElementName ~> Element
	}

	private def pseudo: Rule1[RawSimpleSelector] = rule {
		':' ~!~ (pseudoElement | pseudoClass)
	}

	private def pseudoElement: Rule1[PseudoElement] = rule {
		':' ~!~ CssIdentifier ~> (
			(identifier: CssIdentifier) => {
				val lower = identifier.value.toLowerCase

				PseudoElements.PseudoElements.get(lower) match {
					case Some(element) => PseudoElement(element)
					case None => PseudoElement(CustomPseudoElement(lower))
				}
			}
		)
	}

	private def pseudoClass: Rule1[RawPseudoClass] = rule {
		CssIdentifier ~ run {
			(cursorChar: @switch) match {
				case '(' => '(' ~!~ SingleLineWhitespace ~ functionalPseudoClass ~ SingleLineWhitespace ~ ')'
				case _ => run((identifier: CssIdentifier) =>
					NonFunctional(PseudoClasses.NonFunctionalPseudoClass.get(identifier.value.toLowerCase) match {
						case Some(pseudoClass) => pseudoClass
						case None => CustomPseudoClass(identifier)
					}))
			}
		}
	}


	private def attribute: Rule1[Attribute] = rule {
		('[' ~!~ SingleLineWhitespace ~
			QualifiedAttributeName ~ SingleLineWhitespace ~
			optional(matchTarget) ~ SingleLineWhitespace ~
			optional(modifier) ~ SingleLineWhitespace ~
		']') ~> (Attribute(_, _, _))
	}

	private def matchTarget: Rule1[MatchTarget] = rule {
		attributeMatcher ~ SingleLineWhitespace ~ CssIdentifier ~> MatchTarget
	}

	private def attributeMatcher: Rule1[Matcher] = rule {
		valueMap(AttributeSelector.Matchers)
	}

	private def modifier: Rule1[Modifier] = rule {
		valueMap(AttributeSelector.Modifiers)
	}


	private def functionalPseudoClass: Rule[CssIdentifier :: HNil, RawPseudoClass :: HNil] = rule {
		run((identifier: CssIdentifier) => {
			val normalized = identifier.value.toLowerCase

			PseudoClasses.SubSelectors.get(normalized) match {
				case Some(subSelector) => Selector ~> (RawSubSelector(subSelector, _))
				case None => PseudoClasses.NthPseudoClasses.get(normalized) match {
					case Some(nth) =>
						AnPlusB ~ optional(
							MandatorySingleLineWhitespace ~ Token("of") ~ MandatorySingleLineWhitespace ~ Selector
						) ~> (RawNth(nth, _, _))
					case None => normalized match {
						case PseudoClasses.Dir.name => dirPseudoClass
						case PseudoClasses.Drop.name => dropPseudoClass
						case PseudoClasses.Lang.name => langPseudoClass
						case _ => MISMATCH
					}
				}
			}
		})
	}

	private def dirPseudoClass: Rule1[RawPseudoClass] = rule {
		CssIdentifier ~> (
			(identifier: CssIdentifier) => PseudoClasses.Directionality.get(identifier.value.toLowerCase) match {
				case Some(directionality) => Dir(directionality)
				case None => Dir(UndefinedDirectionality(identifier))
			}
		)
	}

	private def dropPseudoClass: Rule1[RawPseudoClass] = rule {
		(oneOrMore(CssIdentifier).separatedBy(MandatorySingleLineWhitespace) ~> (
			(identifiers: Seq[CssIdentifier]) => {
				val normalized = identifiers.toSet

				if (identifiers.lengthCompare(normalized.size) == 0) {
					if (normalized.forall(PseudoClasses.DropFilter.get(_).isDefined))
						push(Drop(normalized.map((identifier: CssIdentifier) =>
							PseudoClasses.DropFilter(identifier.value.toLowerCase)
						)))
					else failX("one or more of " + PseudoClasses.DropFilter.keys.mkString(", "))
				} else {
					failX("Duplicate drop keywords") // @pedantic
				}
			}
		)) | push(NonFunctional(PseudoClasses.NonFunctional.Drop))
	}

	private def langPseudoClass: Rule1[RawPseudoClass] = rule {
		oneOrMore(
			CssIdentifier |
			(QuotedString ~> ((string: Value.String) => push(ast.CssIdentifier(string.value))))
		).separatedBy(WhitespaceAround(",")) ~> (
			(identifiers: Seq[CssIdentifier]) => {
				Lang(identifiers.toSet)
			}
		)
	}

}
