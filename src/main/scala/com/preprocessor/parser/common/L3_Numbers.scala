package com.preprocessor.parser.common

import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.NumberUnit
import com.preprocessor.ast.NumberUnit.UnitOfMeasure
import org.parboiled2._

trait L3_Numbers { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_AstNode
	with L2_Strings =>

	import L3_Numbers._


	def Number: Rule1[Value.Number] = rule {
		nodeStart ~ (atomic(base ~ optional(exponent) ~ optional(unitOfMeasure)) ~> (createNumber(_, _, _))) ~ nodeEnd
	}

	def UnsignedInteger: Rule1[Int] = rule {
		digits ~> ((digits: String) => digits.toInt)
	}

	private def integral: Rule1[Double] = rule {
		digits ~> ((digits: String) => digits.toDouble)
	}

	private def fractional: Rule1[Double] = rule {
		'.' ~ digits ~> (digitsToFractional(_))
	}

	private def base: Rule1[Double] = rule {
		((Sign ~ integral ~ optional(fractional)) ~> (computeBase(_, _, _))) |
			(Sign ~ fractional ~> ((sign: Int, fractional: Double) => computeBase(sign, 0, Some(fractional))))
	}

	def Sign: Rule1[Int] = rule {
		capture(optional(Signs)) ~> (signToFactor(_))
	}

	private def digits: Rule1[String] = rule {
		capture(oneOrMore(CharPredicate.Digit))
	}

	private def exponent: Rule1[Double] = rule {
		(Exponent ~ Sign ~ digits) ~> ((sign: Int, digits: String) => sign * digits.toDouble)
	}

	private def unitOfMeasure: Rule1[NumberUnit.Unit] = rule {
		('%' ~ push(NumberUnit.Percentage)) |
			capture(oneOrMore(CharPredicate.Alpha)) ~> ((unitOfMeasure: String) => UnitOfMeasure(Map(unitOfMeasure -> 1)))
	}

}


object L3_Numbers {
	val Signs = CharPredicate("+-")
	val Exponent = CharPredicate("eE")


	def signToFactor(sign: String): Int = if (sign == "-") -1 else 1

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
}
