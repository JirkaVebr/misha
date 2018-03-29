package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Value
import com.mishaLang.spec.SelectorSeparator.{Descendant, SelectorListSeparator}

import scala.annotation.tailrec


class SelectorPreprocessor(val rawSelector: RawRuleHead, val parentSelector: Vector[Value.String]) {

	import SelectorPreprocessor._

	private def splitIntoSelectorList(selector: RawRuleHead): Set[RawRuleHead] = {
		@tailrec
		def advance(listSoFar: Set[RawRuleHead], currentHead: RawRuleHead, rest: RawRuleHead): Set[RawRuleHead] =
			if (rest.isEmpty)
				if (currentHead.isEmpty) listSoFar
				else listSoFar + currentHead
			else
				rest.head match {
					case Left(value) =>
						val subSelectors = value.split(SelectorListSeparatorRegex)
						val updatedHead = currentHead :+ Left(subSelectors.head)

						if (subSelectors.length == 1)
							advance(listSoFar, updatedHead, rest.tail)
						else
							advance((listSoFar + updatedHead) ++ subSelectors.tail.map {
								subSelector => Vector(Left(subSelector))
							}.toSet, Vector(), rest.tail)
					case Right(_) =>
						advance(listSoFar, currentHead :+ rest.head, rest.tail)
				}

		advance(Set.empty[RawRuleHead], Vector(), selector)
	}


	def preProcess(): String = {
		val split = splitIntoSelectorList(rawSelector)

		split.map {
			selector => {
				val withParents = selector.head match {
					case Left(_) if parentSelector.nonEmpty => Right(parentSelector) +: Left(Descendant.symbol) +: selector
					case _ => selector
				}
				RuleHeadPreprocessor.explode(withParents)
			}
		}.mkString(SelectorListSeparator.symbol + ' ')
	}

}


object SelectorPreprocessor {

	final val SelectorListSeparatorRegex: String = ",\n"
}
