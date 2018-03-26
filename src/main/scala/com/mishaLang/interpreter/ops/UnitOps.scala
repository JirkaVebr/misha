package com.mishaLang.interpreter.ops

import com.mishaLang.ast.NumberUnit._
import com.mishaLang.spec.units.Angle._
import com.mishaLang.spec.units.AtomicUnit
import com.mishaLang.spec.units.Flex.Flex
import com.mishaLang.spec.units.Frequency.{Frequency, Hertz, KiloHertz}
import com.mishaLang.spec.units.Length._
import com.mishaLang.spec.units.Resolution.Resolution
import com.mishaLang.spec.units.Time.{MiliSecond, Second, Time}

object UnitOps {


	final val RadianFactors: Map[Angle, Double] = Map(
		Degree -> Math.PI / 180d,
		Radian -> 1d,
		Gradian -> Math.PI / 200d,
		Turn -> 2d * Math.PI
	)


	final val PixelFactors: Map[Absolute, Double] = Map(
		Pixel -> 1d,
		CentiMeter -> 96d / 2.54,
		MiliMeter -> (96d / 2.54) * 10d,
		QuarterMiliMeter -> (96d / 2.54) * 40,
		Inch -> 96d,
		Pica -> 16d,
		Point -> 4d / 3d
	)


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


	def convertAngleUnit(value: Double, source: Angle, target: Angle): Double = {
		value * RadianFactors(source) / RadianFactors(target)
	}


	def convertFlexUnit(value: Double, source: Flex, target: Flex): Double =
		value // There's just une flex unit

	def convertFrequencyUnit(value: Double, source: Frequency, target: Frequency): Double =
		(source, target) match {
			case (Hertz, KiloHertz) => value / 1000d
			case (KiloHertz, Hertz) => value * 1000d
			case _ => value // The units must be equal
		}


	def convertLengthUnit(value: Double, sourceUnit: Length, targetUnit: Length): Option[Double] =
		(sourceUnit, targetUnit) match {
			case (source: Absolute, target: Absolute) =>
				Some(value * PixelFactors(source) / PixelFactors(target))
			case _ => None
		}


	def convertResolutionUnit(value: Double, source: Resolution, target: Resolution): Double = ???


	def convertTimeUnit(value: Double, source: Time, target: Time): Double =
		(source, target) match {
			case (Second, MiliSecond) => value * 1000d
			case (MiliSecond, Second) => value / 1000d
			case _ => value // The units must be equal
		}

}