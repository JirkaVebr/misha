package com.preprocessor.parser.selector

import com.preprocessor.ast.{CssIdentifier, MatchTarget}
import com.preprocessor.ast.Selector._
import com.preprocessor.parser.common.{L0_Whitespace, L1_Strings, L2_Numbers}
import com.preprocessor.spec.AttributeSelector.{Matcher, Modifier}
import com.preprocessor.spec.{AttributeSelector, PseudoElements}
import com.preprocessor.spec.PseudoElements.CustomPseudoElement
import org.parboiled2.{StringBuilding, _}

import scala.annotation.switch

trait L5_Selector { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Basics
	with L4_AnPlusB =>

	def Selector: Rule1[Selector] = rule {
		compound
	}

	private def compound: Rule1[Selector] = rule {
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
				case _ => MISMATCH
			}
		}
	}

	private def id: Rule1[Id] = rule {
		'#' ~!~ CssIdentifier ~> Id
	}

	private def actualClass: Rule1[Class] = rule {
		'.' ~!~ CssIdentifier ~> Class
	}

	private def pseudo: Rule1[SimpleSelector] = rule {
		':' ~!~ (pseudoElement | pseudoClass)
	}

	private def pseudoElement: Rule1[PseudoElement] = rule {
		':' ~!~ CssIdentifier ~> (
			(identifier: CssIdentifier) => PseudoElements.pseudoElements.get(identifier) match {
				case Some(element) => PseudoElement(element)
				case None => PseudoElement(CustomPseudoElement(identifier))
			}
		)
	}

	private def pseudoClass: Rule1[PseudoClass] = rule {
		CssIdentifier ~> CustomPseudoClass // TODO
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

}
