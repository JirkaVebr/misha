package com.preprocessor.parser

import com.preprocessor.BaseSpec
import com.preprocessor.ast.Ast.Node
import org.parboiled2.{ParseError, Rule1}

import scala.util.{Failure, Success}

class BaseParserSpec extends BaseSpec {


	protected def getParser(input: String): Parser = new Parser(new IndentDedentParserInput(input))

	protected def parseRule[A <: Node](rule: Rule1[A]): A = {
		rule.run() match {
			case Success(result) => result
			case Failure(failure: ParseError) =>
				println(failure.traces)
				fail()
			case a =>
				fail(a.asInstanceOf[String])
		}
	}
}
