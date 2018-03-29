package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Type._
import com.mishaLang.ast.Language.Value.Tuple2
import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.interpreter.EnvWithValue
import com.mishaLang.interpreter.validators.NumberValidator
import com.mishaLang.spec.units.Angle.Angle
import com.mishaLang.spec.units.Flex.Flex
import com.mishaLang.spec.units.Frequency.Frequency
import com.mishaLang.spec.units.Length.Length
import com.mishaLang.spec.units.Resolution.Resolution
import com.mishaLang.spec.units.Time.Time

object TypeOps {

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
				Some(value).filter(UnitOps.isUnit[Length])
			case Color =>
				if (value.isInstanceOf[Value.Color]) Some(value) else None
			case Literal(literalValue) =>
				if (value == literalValue) Some(value) else None
			case Type.Scalar =>
				value match {
					case number: Value.Number if NumberValidator.isScalar(number) => Some(number)
					case _ => None
				}
			case Type.Percentage => value match {
				case number: Value.Number if NumberValidator.isPercentage(number) => Some(number)
				case _ => None
			}
			case Type.Angle =>
				Some(value).filter(UnitOps.isUnit[Angle])
			case Type.Flex =>
				Some(value).filter(UnitOps.isUnit[Flex])
			case Type.Time =>
				Some(value).filter(UnitOps.isUnit[Time])
			case Type.Resolution =>
				Some(value).filter(UnitOps.isUnit[Resolution])
			case Type.Boolean =>
				if (value.isInstanceOf[Value.Boolean]) Some(value) else None
			case Flag =>
				if (value.isInstanceOf[Value.Flag]) Some(value) else None
			case Type.Frequency =>
				Some(value).filter(UnitOps.isUnit[Frequency])
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
			case Type.List(of) =>
				value match {
					case list: Value.List =>
						if (of == Type.Any) Some(value)
						else {
							val assignedVector = list.values.flatMap {
								item => canBeAssignedTo(item, of)
							}
							if (assignedVector.length == list.values.length) Some(Value.List(assignedVector))
							else None
						}
					case _ => None
				}
			case Map(key, value, mandatoryEntries) => ???
			case Formula(subtype) => ???
		}
		case TypeAlias(name) => state.environment.lookup(name) match {
			case Some(aliasedType) => canBeAssignedTo(value, aliasedType)
			case None => None
		}
		case Any => Some(value)
	}
}
