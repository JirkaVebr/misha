package com.mishaLang.interpreter.typing

import com.mishaLang.ast.Language.Type.{Any, Color, Flag, Formula, Function, Literal, Map, Subtraction, TypeAlias, Union}
import com.mishaLang.ast.Language.Value.{Dimensioned, Number, Tuple2}
import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.ast.NumberUnit.{Atomic, Percentage, SimpleUnit}
import com.mishaLang.interpreter.EnvWithValue
import com.mishaLang.spec.units.Angle.Angle
import com.mishaLang.spec.units.AtomicUnit
import com.mishaLang.spec.units.Flex.Flex
import com.mishaLang.spec.units.Frequency.Frequency
import com.mishaLang.spec.units.Length.Length
import com.mishaLang.spec.units.Resolution.Resolution
import com.mishaLang.spec.units.Time.Time

import scala.reflect.ClassTag

object Typing {

	/**
		* The idea of this method is to essentially just check whether a value satisfies a given type. However, the result
		* isn't being reported as a Boolean, but rather as another value because the argument value might be very close to
		* the expected type but not quite it, in which this function performs a coercion as a means of convenience for the
		* user. For instance, if the value is "Scalar(0)" but the expected type is "Angle", this method will concede that
		* the value is "close enough" and return an appropriate "Dimensioned(…)" value.
		*/
	def canBeAssignedTo(value: Value.Value, theType: Type.Any)(implicit state: EnvWithValue): Option[Value.Value] = theType match {
		case primitive: Type.Primitive => primitive match {
			case Type.String =>
				if (value.isInstanceOf[Value.String]) Some(value) else None
			case Type.Length =>
				Some(value).filter(isUnit[Length])
			case Color =>
				if (value.isInstanceOf[Value.Color]) Some(value) else None
			case Literal(literalValue) =>
				if (value == literalValue) Some(value) else None
			case Type.Scalar =>
				if (value.isInstanceOf[Value.Scalar]) Some(value) else None
			case Type.Percentage => value match {
				case dimensioned: Dimensioned if dimensioned.unit == Percentage => Some(value)
				case _ => None
			}
			case Type.Angle =>
				Some(value).filter(isUnit[Angle])
			case Type.Flex =>
				Some(value).filter(isUnit[Flex])
			case Type.Time =>
				Some(value).filter(isUnit[Time])
			case Type.Resolution =>
				Some(value).filter(isUnit[Resolution])
			case Type.Boolean =>
				if (value.isInstanceOf[Value.Boolean]) Some(value) else None
			case Flag =>
				if (value.isInstanceOf[Value.Flag]) Some(value) else None
			case Type.Frequency =>
				Some(value).filter(isUnit[Frequency])
			case Type.Unit =>
				if (value == Value.Unit) Some(value) else None
		}
		case composite: Type.Composite => composite match {
			case Union(subs) =>
				val correctType = subs.find(canBeAssignedTo(value, _).isDefined)
				// TODO this is inefficient - calling canBeAssignedTo twice for the right value
				if (correctType.isDefined) canBeAssignedTo(value, correctType.get)
				else None
			case Function(arguments, output) => ???
			case Type.Tuple2(first, second) => value match {
				case Tuple2(firstValue, secondValue) => // TODO
					val firstAssigned = canBeAssignedTo(firstValue, first)
					val secondAssigned = canBeAssignedTo(secondValue, second)
					if (firstAssigned.isDefined && secondAssigned.isDefined)
						Some(Tuple2(firstAssigned.get, secondAssigned.get))
					else None
				case _ => None
			}
			case Subtraction(minuend, subtrahend) =>
				val minuendValue = canBeAssignedTo(value, minuend)
				if (minuendValue.isDefined && canBeAssignedTo(value, subtrahend).isEmpty) minuendValue else None
			case Type.List(_) =>
				if (value.isInstanceOf[Value.List]) Some(value) else None // TODO
			case Map(key, value, mandatoryEntries) => ???
			case Formula(subtype) => ???
		}
		case TypeAlias(name) => ???
		case Any => Some(value)
	}


	private def isUnit[U <: AtomicUnit : ClassTag](value: Value.Value): Boolean = value match {
		case number: Number => number match {
			case Dimensioned(_, dimensionedUnit) => dimensionedUnit match {
				case simple: SimpleUnit => simple match {
					case Atomic(atomicUnit) =>
						implicitly[ClassTag[U]].runtimeClass.isInstance(atomicUnit)
					case _ => false
				}
				case _ => false
			}
			case _ => false
		}
		case _ => false
	}
}
