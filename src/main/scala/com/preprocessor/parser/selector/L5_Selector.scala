package com.preprocessor.parser.selector

import com.preprocessor.ast
import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.Selector._
import com.preprocessor.ast.{CssIdentifier, MatchTarget}
import com.preprocessor.parser.common.{L0_Whitespace, L1_Strings, L2_Numbers}
import com.preprocessor.spec.AttributeSelector.{Matcher, Modifier}
import com.preprocessor.spec.PseudoClasses.NonFunctional.CustomPseudoClass
import com.preprocessor.spec.PseudoClasses.UndefinedDirectionality
import com.preprocessor.spec.PseudoElements.CustomPseudoElement
import com.preprocessor.spec.SelectorCombinator._
import com.preprocessor.spec.{AttributeSelector, PseudoClasses, PseudoElements}
import org.parboiled2.{StringBuilding, _}
import shapeless.{::, HNil}

import scala.annotation.switch

/**
	* TODO special char escaping
	* @see https://drafts.csswg.org/selectors-4/#case-sensitive
	*/
trait L5_Selector { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Basics
	with L4_AnPlusB =>

	def Selector: Rule1[Selector] = rule {
		selectorList
	}

	private def selectorList: Rule1[Selector] = rule {
		oneOrMore(complexSelector).separatedBy(AnyWhitespaceAround(",")) ~> (
			(selectors: Seq[Selector]) =>
				if (selectors.lengthCompare(1) == 0) selectors.head
				else RawSelectorList(selectors)
		)
	}

	private def complexSelector: Rule1[Selector] = rule { // Right associative
		compoundSelector ~ zeroOrMore(combinator ~ complexSelector ~> (
			(left: Selector, combinator: Combinator, right: Selector) => Complex(combinator, left, right)
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
	private def compoundSelector: Rule1[Selector] = rule {
		oneOrMore(simpleSelector) ~> (
			(selectors: Seq[SimpleSelector]) =>
				if (selectors.lengthCompare(1) == 0) selectors.head
				else RawCompound(selectors)
		)
	}

	private def simpleSelector: Rule1[SimpleSelector] = rule {
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

	private def pseudo: Rule1[SimpleSelector] = rule {
		':' ~!~ (pseudoElement | pseudoClass)
	}

	private def pseudoElement: Rule1[PseudoElement] = rule {
		':' ~!~ CssIdentifier ~> (
			(identifier: CssIdentifier) => {
				val lower = identifier.value.toLowerCase

				PseudoElements.pseudoElements.get(lower) match {
					case Some(element) => PseudoElement(element)
					case None => PseudoElement(CustomPseudoElement(lower))
				}
			}
		)
	}

	private def pseudoClass: Rule1[PseudoClass] = rule {
		CssIdentifier ~ run {
			(cursorChar: @switch) match {
				case '(' => '(' ~!~ SingleLineWhitespace ~ functionalPseudoClass ~ SingleLineWhitespace ~ ')'
				case _ => run((identifier: CssIdentifier) =>
					NonFunctional(PseudoClasses.nonFunctionalPseudoClass.get(identifier.value.toLowerCase) match {
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
		valueMap(AttributeSelector.matchers)
	}

	private def modifier: Rule1[Modifier] = rule {
		valueMap(AttributeSelector.modifiers)
	}


	private def functionalPseudoClass: Rule[CssIdentifier :: HNil, PseudoClass :: HNil] = rule {
		run((identifier: CssIdentifier) => {
			val normalized = identifier.value.toLowerCase

			PseudoClasses.subSelectors.get(normalized) match {
				case Some(subSelector) => Selector ~> (SubSelector(subSelector, _))
				case None => PseudoClasses.nthPseudoClasses.get(normalized) match {
					case Some(nth) =>
						AnPlusB ~ optional(
							MandatorySingleLineWhitespace ~ Token("of") ~ MandatorySingleLineWhitespace ~ Selector
						) ~> (Nth(nth, _, _))
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

	private def dirPseudoClass: Rule1[PseudoClass] = rule {
		CssIdentifier ~> (
			(identifier: CssIdentifier) => PseudoClasses.directionality.get(identifier.value.toLowerCase) match {
				case Some(directionality) => Dir(directionality)
				case None => Dir(UndefinedDirectionality(identifier))
			}
		)
	}

	private def dropPseudoClass: Rule1[PseudoClass] = rule {
		(oneOrMore(CssIdentifier).separatedBy(MandatorySingleLineWhitespace) ~> (
			(identifiers: Seq[CssIdentifier]) => {
				val normalized = identifiers.toSet

				if (identifiers.lengthCompare(normalized.size) == 0) {
					if (normalized.forall(PseudoClasses.dropFilter.get(_).isDefined))
						push(Drop(normalized.map((identifier: CssIdentifier) =>
							PseudoClasses.dropFilter(identifier.value.toLowerCase)
						)))
					else failX("one or more of " + PseudoClasses.dropFilter.keys.mkString(", "))
				} else {
					failX("Duplicate drop keywords") // @pedantic
				}
			}
		)) | push(NonFunctional(PseudoClasses.NonFunctional.Drop))
	}

	private def langPseudoClass: Rule1[PseudoClass] = rule {
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
