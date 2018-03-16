package com.preprocessor.interpreter

import RuleContext.{AtRule, RuleSelector}
import com.preprocessor.ast.Selector.SelectorList
import Symbol.RuleContextSymbol
import com.preprocessor.emitter.SelectorEmitter


/**
	* A rule head can look something like this:
	*     .foo-{1 + 1, 3 + 3}-bar
	* Which, in the end, should generate
	*     .foo-2-bar, .foo-6-bar
	*
	* @param rawRuleHead Normalized, meaning that there are no sequences of Left(string)
	*/
class RuleHeadPreprocessor(val rawRuleHead: RawRuleHead, val environment: Environment) {

	val context: Option[RuleContextSymbol.Value] = environment.lookupContext()
	val contextVector: Option[Vector[String]] = initializeContextVector()


	def preProcess(): String = {
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

		rec(rawRuleHead).mkString(",\n").trim
	}


	private def initializeContextVector(): Option[Vector[String]] = context match {
		case Some(ruleContext) => ruleContext match {
			case RuleSelector(selector) => selector match {
				case SelectorList(selectors) => Some(
					selectors.toVector.map(SelectorEmitter.emit(_)(StringBuilder.newBuilder).toString)
				)
				case _ => Some(Vector(SelectorEmitter.emit(selector)(StringBuilder.newBuilder).toString))
			}
			case _: AtRule => None // TODO
			case _ => None // TODO
		}
		case None => None
	}

}
