package com.preprocessor.parser

import com.preprocessor.BaseSpec
import com.preprocessor.ast.Ast.Node
import org.parboiled2.{ParseError, Rule1}

import scala.util.{Failure, Success}

class BaseParserSpec extends BaseSpec {


	protected def parseRule[A <: Node](input: String, rule: LanguageParser => Rule1[A]): A = {
		// __run() isn't public API, so this may break
		// @see https://groups.google.com/forum/#!topic/parboiled-user/uwcy6MVZV5s
		val parser = LanguageParser.create(input)
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
