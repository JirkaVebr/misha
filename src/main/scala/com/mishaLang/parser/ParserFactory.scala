package com.mishaLang.parser

trait ParserFactory[P <: org.parboiled2.Parser] {

	def create(input: String): P
}
