package com.mishaLang.parser

import scala.util.Try


trait ParserOf[R] {

	def apply(input: String): Try[R]
}
