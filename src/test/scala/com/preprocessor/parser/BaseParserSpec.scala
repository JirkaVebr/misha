package com.preprocessor.parser

import com.preprocessor.BaseSpec
import com.preprocessor.parser.language.LanguageParser
import com.preprocessor.parser.selector.SelectorParser
import org.parboiled2.{ParseError, Rule1}

import scala.util.{Failure, Success}

class BaseParserSpec extends BaseSpec {

	protected def parseLanguageRule[A](input: String, rule: LanguageParser => Rule1[A]): A =
		parseRule[LanguageParser, A](LanguageParser, input, rule)

	protected def parseSelectorRule[A](input: String, rule: SelectorParser => Rule1[A]): A =
		parseRule[SelectorParser, A](SelectorParser, input, rule)


	protected def parseRule[P <: org.parboiled2.Parser, A](parserFactory: ParserFactory[P], input: String, rule: P => Rule1[A]): A = {
		// __run() isn't public API, so this may break
		// @see https://groups.google.com/forum/#!topic/parboiled-user/uwcy6MVZV5s
		val parser: P = parserFactory.create(input)
		parser.__run(rule(parser)) match {
			case Success(result) => result
			case Failure(failure: ParseError) =>
				println(failure.format(parser))
				fail()
			case a =>
				println(a)
				fail()
		}
	}
}
