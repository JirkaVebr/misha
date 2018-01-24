package com.preprocessor.parser

import org.parboiled2._


trait L1_Literals { this: org.parboiled2.Parser
	with StringBuilding
	with Whitespace
	with L0_Basics =>

	import CharPredicate.HexDigit
	import L1_Literals._

	//def Literal: Rule1


	def string: Rule1[String] = rule {
		quotedString | unquotedString
	}

	def quotedString: Rule1[String] = rule {
		capture("")
	}

	def unquotedString: Rule1[String] = rule {
		capture("")
	}



	def characters: Rule0 = rule {
		zeroOrMore(normalChar | '\\' ~ escapedChar)
	}

	def normalChar: Rule0 = rule {
		!StringDelimiterOrBackslash ~ ANY ~ appendSB()
	}

	def escapedChar: Rule0 = rule (
		StringDelimiterOrBackslash ~ appendSB()
			| 'n' ~ appendSB('\n')
			| 'r' ~ appendSB('\r')
			| 't' ~ appendSB('\t')
			| unicodeSequence ~> { (code: Int) => sb.append(code.asInstanceOf[Char]); () }
	)

	def unicodeSequence: Rule1[Int] = rule {
		capture(oneOrMore(HexDigit)) ~> ((hexDigits: String) => java.lang.Integer.parseInt(hexDigits, 16))
	}

}

object L1_Literals {
	val StringDelimiterOrBackslash = CharPredicate("\"\\'")
}
