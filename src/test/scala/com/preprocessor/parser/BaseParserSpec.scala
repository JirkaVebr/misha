package com.preprocessor.parser

import com.preprocessor.BaseSpec
import com.preprocessor.ast.Ast
import org.parboiled2.ParseError

import scala.util.{Failure, Success}

class BaseParserSpec extends BaseSpec {


	protected def getParser(input: String): Parser = new Parser(new IndentDedentParserInput(input))

	protected def parseProgram(input: String): Ast.Node = {
		val parser = getParser(input)

		parser.Program.run() match {
			case Success(program) => program
			case Failure(error: ParseError) =>
				fail("Parse error: " + error.format(input))
			case Failure(error) =>
				throw error
		}
	}
}
