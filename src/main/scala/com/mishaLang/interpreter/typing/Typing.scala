package com.mishaLang.interpreter.typing

import com.mishaLang.ast.Language.Type.{Any, Color, Flag, Formula, Function, Literal, Map, Subtraction, TypeAlias, Union}
import com.mishaLang.ast.Language.Value.{Color, Composite, Dimensioned, Flag, Number, Percentage, Primitive, Scalar, String, Tuple2, Unit}
import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.ast.NumberUnit.{Atomic, SimpleUnit}
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

	def getType(value: Value.Value)/*(implicit environment: Environment)*/: Type.Any = value match {
		case Unit => Type.Unit
		case primitive: Primitive => primitive match {
			case number: Number => number match {
				case Dimensioned(value, unit) => sys.error("todo") // TODO
				case Scalar(_) => Type.Scalar
				case Percentage(_) => Type.Percentage
			}
			case Value.Boolean(_) => Type.Boolean
			case String(_) => Type.String
			case _: Color => Type.Color
			case _: Flag => Type.Flag
		}
		case composite: Composite => composite match {
			case Tuple2(first, second) => Type.Tuple2(getType(first), getType(second))
			case Value.List(values) =>  sys.error("todo") // TODO
			case _ =>  sys.error("todo") // TODO
		}
	}


	def canBeAssignedTo(value: Value.Value, theType: Type.Any)(implicit state: EnvWithValue): Boolean = theType match {
		case primitive: Type.Primitive => primitive match {
			case Type.String => value.isInstanceOf[Value.String]
			case Type.Length => isUnit[Length](value)
			case Color => value.isInstanceOf[Value.Color]
			case Literal(literalValue) => value == literalValue
			case Type.Scalar => value.isInstanceOf[Value.Scalar]
			case Type.Percentage => value.isInstanceOf[Value.Percentage] // TODO?
			case Type.Angle => isUnit[Angle](value)
			case Type.Flex => isUnit[Flex](value)
			case Type.Time => isUnit[Time](value)
			case Type.Resolution => isUnit[Resolution](value)
			case Type.Boolean => value.isInstanceOf[Value.Boolean]
			case Flag => value.isInstanceOf[Value.Flag]
			case Type.Frequency => isUnit[Frequency](value)
			case Type.Unit => value == Value.Unit
		}
		case composite: Type.Composite => composite match {
			case Union(subs) => subs.exists(canBeAssignedTo(value, _))
			case Function(arguments, output) => ???
			case Type.Tuple2(first, second) => value match {
				case Tuple2(firstValue, secondValue) => canBeAssignedTo(firstValue, first) && canBeAssignedTo(secondValue, second)
				case _ => false
			}
			case Subtraction(minuend, subtrahend) => canBeAssignedTo(value, minuend) && !canBeAssignedTo(value, subtrahend)
			case Type.List(_) => value.isInstanceOf[Value.List] // TODO
			case Map(key, value, mandatoryEntries) => ???
			case Formula(subtype) => ???
		}
		case TypeAlias(name) => ???
		case Any => true
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
