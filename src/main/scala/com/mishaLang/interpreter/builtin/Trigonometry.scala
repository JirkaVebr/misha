package com.mishaLang.interpreter.builtin

import com.mishaLang.ast.Language.Value.{Native, Value}
import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.ast.NumberUnit.Atomic
import com.mishaLang.error.CompilerError
import com.mishaLang.interpreter.Symbol
import com.mishaLang.interpreter.ops.UnitOps
import com.mishaLang.spec.units.Angle.{Angle, Radian}

import scala.util.{Failure, Success}

object Trigonometry {

	lazy final val Trigonometry: Map[String, Symbol.ValueSymbol#Value] = Map(
		"sin" -> Sin,
		"cos" -> Cos,
		"tan" -> Tan
	)


	private def toRadians(value: Value.Value): Option[Double] = value match {
		case Value.Scalar(scalar) => Some(scalar)
		case Value.Dimensioned(dimensioned, unit) =>
			unit match {
				case Atomic(atomicUnit) => atomicUnit match {
					case angle: Angle => Some(UnitOps.convertAngleUnit(dimensioned, angle, Radian))
					case _ => None
				}
				case _ => None
			}
		case _ => None
	}


	private def getTrigonometricFunction(underlying: (Double) => Double): Native =
		Native(Vector(Type.Union(Set(
			Type.Scalar, Type.Angle
		))), (arguments: Vector[Value]) => {
			toRadians(arguments(0)) match {
				case Some(radians) => Success(Value.Scalar(underlying(radians)))
				case None => Failure(CompilerError("This should never happen"))
			}
		})


	final val Sin = getTrigonometricFunction(Math.sin)
	final val Cos = getTrigonometricFunction(Math.cos)
	final val Tan = getTrigonometricFunction(Math.tan)

}
