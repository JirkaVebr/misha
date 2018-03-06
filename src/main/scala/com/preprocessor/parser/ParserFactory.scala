package com.preprocessor.parser

trait ParserFactory[P <: org.parboiled2.Parser] {

	def create(input: String): P
}
