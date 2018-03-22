package com.mishaLang.parser

import com.mishaLang.BaseSpec
import com.mishaLang.parser.language.LanguageParser
import com.mishaLang.parser.ruleHead.SelectorParser
import org.parboiled2.{ParseError, Rule1}

import scala.util.{Failure, Success, Try}

class BaseParserSpec extends BaseSpec {

	protected def parseLanguageRule[A](input: String, rule: LanguageParser => Rule1[A]): A =
		parseRule[LanguageParser, A](LanguageParser, input, rule).get

	protected def parseSelectorRule[A](input: String, rule: SelectorParser => Rule1[A]): A =
		parseRule[SelectorParser, A](SelectorParser, input, rule).get

	protected def parseRule[P <: org.parboiled2.Parser, A](parserFactory: ParserFactory[P], input: String, rule: P => Rule1[A]): Try[A] = {
		// __run() isn't public API, so this may break
		// @see https://groups.google.com/forum/#!topic/parboiled-user/uwcy6MVZV5s
		val parser: P = parserFactory.create(input)
		val result = parser.__run(rule(parser))
		result match {
			case Failure(exception) => exception match {
				case e: ParseError =>
					//println(e.traces)
					//println(e.format(parser))
					result
				case _ => result
			}
			case _ => result
		}
	}
}
