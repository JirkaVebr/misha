package com.preprocessor

import com.preprocessor.interpreter.Evaluator
import com.preprocessor.parser.{IndentDedentParserInput, Parser}
import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success}


object Main extends App {

	val testSource = Source.fromFile("src/input/test.tss").mkString
	var parseResult = new Parser(new IndentDedentParserInput(testSource)).Program.run()
	//var parseResult = new TestParser(testSource).Expression.run()


	parseResult match {
		case Success(program) =>
			println("The ast is:")
			println(program)
			//println("The result is " + new Evaluator(program).evaluate())
		case Failure(error: ParseError) => println("Parse error: " + error.format(testSource))
		case Failure(error) => println("Unexpected parse error: " + error)
	}

}
