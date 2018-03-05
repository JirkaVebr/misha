package com.preprocessor.parser

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{Color, Duplicate, Flag, Important, Primitive, Rgba}
import com.preprocessor.ast.NumberUnit
import com.preprocessor.ast.NumberUnit.UnitOfMeasure
import com.preprocessor.spec.ColorKeywords
import org.parboiled2._


trait L2_Literals { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings =>

	import CharPredicate.HexDigit
	import L2_Literals._


	def Literal: Rule1[Primitive] = rule {
		Flag | boolean | number | QuotedString | color | UnquotedString
	}

	def Flag: Rule1[Flag] = rule {
		'!' ~ ((Token("important") ~ push(Important)) |
			(Token("duplicate") ~ push(Duplicate)))
	}

	private def boolean: Rule1[Value.Boolean] = rule {
		(capture(atomic("true")) | capture(atomic("false"))) ~>
			((literal: String) => Value.Boolean(literal.charAt(0) == 't'))
	}


	/* NUMBERS */

	private def number: Rule1[Value.Number] = rule {
		atomic(base ~ optional(exponent) ~ optional(unitOfMeasure)) ~> (createNumber(_, _, _))
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

	private def unitOfMeasure: Rule1[NumberUnit.Unit] = rule {
		('%' ~ push(NumberUnit.Percentage)) |
		capture(oneOrMore(CharPredicate.Alpha)) ~> ((unitOfMeasure: String) => UnitOfMeasure(Map(unitOfMeasure -> 1)))
	}



	/* COLORS */

	private def color: Rule1[Color] = rule {
		hexColor | colorKeyword
	}

	private def hexColor: Rule1[Color] = rule {
		'#' ~ capture(
			8.times(HexDigit) |
			6.times(HexDigit) |
			(3 to 4).times(HexDigit)
		) ~> (convertHexToColor(_))
	}

	private def colorKeyword: Rule1[Color] = rule {
		valueMap(ColorKeywords.map, ignoreCase = true)
	}


}

object L2_Literals {
	val Signs = CharPredicate("+-")
	val Exponent = CharPredicate("eE")


	private def signToFactor(sign: String): Int = if (sign == "-") -1 else 1

	private def digitsToFractional(digits: String): Double = {
		val digitsCount = digits.length
		if (digitsCount == 0) 0
		else digits.toDouble / Math.pow(10, digitsCount)
	}

	private def computeBase(sign: Int, integral: Double, fractional: Option[Double]): Double =
		sign * (integral + fractional.getOrElse(0.0d))

	private def getNumericValue(base: Double, exponent: Option[Double]) =
		if (exponent.isEmpty) base else base * Math.pow(10, exponent.get)

	private def createNumber(base: Double, exponent: Option[Double], unitOfMeasure: Option[NumberUnit.Unit]): Value.Number = {
		val value = getNumericValue(base, exponent)
		unitOfMeasure match {
			case Some(unit) => unit match {
				case unit: UnitOfMeasure => Value.Dimensioned(value, unit)
				case NumberUnit.Percentage => Value.Percentage(value)
			}
			case None => Value.Scalar(value)
		}
	}

	private def convertHexToColor(hex: String): Rgba = {
		val isShort = hex.length == 3 || hex.length == 4
		val hasAlpha = hex.length == 4 || hex.length == 8
		val h2d = (c1: Char, c2: Char) => java.lang.Integer.parseInt(s"$c1$c2", 16)
		val norm: Vector[Int] =
			if (isShort) hex.toVector.map((c: Char) => h2d(c, c))
			else hex.toVector.grouped(2).map((pxs: Vector[Char]) => h2d(pxs(0), pxs(1))).toVector

		Rgba(norm(0), norm(1), norm(2), if (hasAlpha) norm(3) else 255)
	}
}
