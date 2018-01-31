package com.preprocessor.parser

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{Color, Flag, Important, Primitive}
import com.preprocessor.spec.ColorKeywords
import org.parboiled2._


trait L1_Literals { this: org.parboiled2.Parser
	with StringBuilding
	with Whitespace
	with L0_Basics =>

	import CharPredicate.HexDigit
	import L1_Literals._


	def Literal: Rule1[Primitive] = rule {
		flag | boolean | number | colorKeyword
	}

	private def flag: Rule1[Flag] = rule {
		atomic("!important") ~> (() => Important)
	}

	private def boolean: Rule1[Value.Boolean] = rule {
		(capture("true") | capture("false")) ~> ((literal: String) => Value.Boolean(literal == "true"))
	}


	/* NUMBERS */

	private def number: Rule1[Value.Number] = rule {
		(base ~ optional(exponent)) ~> (computeExponential(_, _))
	}

	private def integral: Rule1[Double] = rule {
		digits ~> ((digits: String) => digits.toDouble)
	}

	private def fractional: Rule1[Double] = rule {
		'.' ~ digits ~> (digitsToFractional(_))
	}

	private def base: Rule1[Double] = rule {
		((sign ~ integral ~ optional(fractional)) ~> (computeBase(_, _, _))) |
		(sign ~ fractional ~> ((sign: Int, fractional: Double) => computeBase(sign, 0, Some(fractional))))
	}

	private def sign: Rule1[Int] = rule {
		capture(optional(Signs)) ~> (signToFactor(_))
	}

	private def digits: Rule1[String] = rule {
		capture(oneOrMore(CharPredicate.Digit))
	}

	private def exponent: Rule1[Double] = rule {
		(Exponent ~ sign ~ digits) ~> ((sign: Int, digits: String) => sign * digits.toDouble)
	}



	/* COLORS */

	private def colorKeyword: Rule1[Color] = rule {
		valueMap(ColorKeywords.map, ignoreCase = true)
	}


	/* STRINGS */

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
	val Signs = CharPredicate("+-")
	val Exponent = CharPredicate("eE")
	val StringDelimiterOrBackslash = CharPredicate("\"\\'")


	private def signToFactor(sign: String): Int = if (sign == "-") -1 else 1

	private def digitsToFractional(digits: String): Double = {
		val digitsCount = digits.length
		if (digitsCount == 0) 0
		else digits.toDouble / Math.pow(10, digitsCount)
	}

	private def computeBase(sign: Int, integral: Double, fractional: Option[Double]): Double =
		sign * (integral + fractional.getOrElse(0.0d))

	private def computeExponential(base: Double, exponent: Option[Double]): Value.Number =
		Value.Number(Math.pow(base, exponent.getOrElse(1.0d)))
}
