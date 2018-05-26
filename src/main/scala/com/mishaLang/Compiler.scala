package com.mishaLang

import com.mishaLang.emitter.Emitter
import com.mishaLang.error.{NativeError, ProgramError, SelectorError}
import com.mishaLang.interpreter.Interpreter
import com.mishaLang.parser.language.LanguageParser
import org.parboiled2._

import scala.io.Source
import scala.util.{Failure, Success}

class Compiler(val input: String) {

	protected val parser: LanguageParser = LanguageParser.create(input)


	def compile(): String = {
		parser.Program.run() match {
			case Success(value) =>
				val interpreter = new Interpreter(value)
				interpreter.runProgram() match {
					case Success(finalEnvironment) =>
						val emitter = new Emitter(finalEnvironment.environment)
						emitter.emit().mkString
					case Failure(error) =>
						error match {
							case e: ProgramError[_] => e.errorCode.message()
							case e: SelectorError => e.errorCode.message()
							case e: NativeError => e.errorCode.message()
							case _ => "Unexpected interpreter error"
						}
				}
			case Failure(error: ParseError) =>
				"Parse error: " + error.format(parser)
			case Failure(error) =>
				"Unexpected parse error: " + error // TODO
		}
	}

}


object Compiler {

	def compileString(input: String): String =
		new Compiler(input).compile()

	def compileFile(fileName: String): String =
		new Compiler(Source.fromFile(fileName).mkString).compile()
}
