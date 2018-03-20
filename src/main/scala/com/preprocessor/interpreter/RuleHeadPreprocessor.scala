package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Term.ParentSelector
import com.preprocessor.ast.RuleHead
import com.preprocessor.ast.Selector._
import com.preprocessor.spec.SelectorCombinator
import com.preprocessor.spec.SelectorCombinator.Descendant


/**
	* A rule head can look something like this:
	*     .foo-{1 + 1, 3 + 3}-bar
	* Which, in the end, should generate
	*     .foo-2-bar, .foo-6-bar
	*
	*/
object RuleHeadPreprocessor {

	def explode(rawRuleHead: RawRuleHead): String = {
		def rec(ruleHead: Vector[RawRuleHeadComponent]): Vector[String] =
			if (ruleHead.isEmpty)
				Vector.empty
			else {
				val (head, rest) = (ruleHead.head, rec(ruleHead.tail))
				head match {
					case Left(string) =>
						if (rest.isEmpty) Vector(string)
						else rest.map(string + _)
					case Right(values) =>
						values.flatMap(
							stringValue =>
								if (rest.isEmpty) Vector(stringValue.value)
								else rest.map(stringValue.value + _)
						)
				}
			}

		rec(rawRuleHead).mkString(", ").trim
	}


	def isParentImplicit(ruleHead: RuleHead): Boolean =
		ruleHead.headOption match {
			case Some(head) => head match {
				case Left(_) => true
				case Right(expressions) => expressions == ParentSelector
			}
			case None => true // Somewhat arbitrary. Empty ruleHead won't make it past the parser anyway though.
		}


	def prependImplicitParent(parentSelector: NormalizedSelector, selector: NormalizedSelector): NormalizedSelector = {
		def prependSingleSelector(nonListParent: ComplexComponent): Set[ComplexComponent] = {
			selector match {
				case SelectorList(childSelectors) => childSelectors.map(Complex(Descendant, nonListParent, _))
				case complexComponent: ComplexComponent => Set(Complex(Descendant, nonListParent, complexComponent))
			}
		}

		parentSelector match {
			case SelectorList(selectors) =>
				SelectorList(selectors.flatMap(prependSingleSelector))
			case complexComponent: ComplexComponent =>
				val withParent = prependSingleSelector(complexComponent)
				if (withParent.size == 1) withParent.head
				else SelectorList(withParent)
		}
	}

}
