package com.mishaLang.interpreter.ops

import com.mishaLang.ast.NumberUnit._
import com.mishaLang.spec.units.Angle._
import com.mishaLang.spec.units.AtomicUnit
import com.mishaLang.spec.units.Flex.Flex
import com.mishaLang.spec.units.Frequency.Frequency
import com.mishaLang.spec.units.Length.Length
import com.mishaLang.spec.units.Resolution.Resolution
import com.mishaLang.spec.units.Time.{MiliSecond, Second, Time}

object UnitOps {


	def convert(value: Double, source: UnitOfMeasure, target: UnitOfMeasure): Option[Double] =
		source match {
			case simple: SimpleUnit => simple match {
				case Atomic(unit) =>
					target match {
						case Atomic(targetAtomic) => convertAtomicUnit(value, unit, targetAtomic)
						case _ => None
					}
				case Percentage =>
					target match {
						case Percentage => Some(value)
						case _ => None
					}
			}
			case _: ComplexUnit => None // TODO this may otherwise be valid for Hz ←→ s^-1
		}


	def convertAtomicUnit(value: Double, sourceUnit: AtomicUnit, targetUnit: AtomicUnit): Option[Double] =
		(sourceUnit, targetUnit) match {
			case (source: Length, target: Length) => convertLengthUnit(value, source, target)
			case (source: Time, target: Time) => Some(convertTimeUnit(value, source, target))
			case (source: Angle, target: Angle) => Some(convertAngleUnit(value, source, target))
			case (source: Resolution, target: Resolution) => Some(convertResolutionUnit(value, source, target))
			case (source: Frequency, target: Frequency) => Some(convertFrequencyUnit(value, source, target))
			case (source: Flex, target: Flex) => Some(convertFlexUnit(value, source, target))
			case (_, _) => None
		}


	def convertAngleUnit(value: Double, source: Angle, target: Angle): Double =
		value * (source match {
			case Degree =>
				target match {
					case Degree => 1d
					case Gradian => 10d / 9d
					case Radian => Math.PI / 180d
					case Turn => 1d / 360d
				}
			case Gradian =>
				target match {
					case Degree => 9d / 10d
					case Gradian => 1d
					case Radian => Math.PI / 200d
					case Turn => 1d / 400d
				}
			case Radian =>
				target match {
					case Degree => 180d / Math.PI
					case Gradian => 200d / Math.PI
					case Radian => 1d
					case Turn => 1d / (2d * Math.PI)
				}
			case Turn =>
				target match {
					case Degree => 360d
					case Gradian => 400d
					case Radian => 2d * Math.PI
					case Turn => 1d
				}
		})


	def convertFlexUnit(value: Double, source: Flex, target: Flex): Double =
		value // There's just une flex unit


	def convertFrequencyUnit(value: Double, source: Frequency, target: Frequency): Double = ???


	def convertLengthUnit(value: Double, source: Length, target: Length): Option[Double] = ???


	def convertResolutionUnit(value: Double, source: Resolution, target: Resolution): Double = ???


	def convertTimeUnit(value: Double, source: Time, target: Time): Double =
		(source, target) match {
			case (Second, MiliSecond) => value * 1000d
			case (MiliSecond, Second) => value / 1000d
			case _ => value // The units must be equal
		}

}
