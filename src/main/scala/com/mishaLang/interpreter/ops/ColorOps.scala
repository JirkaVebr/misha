package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.ast.Language.Value.{Native, Rgba, Value}
import com.mishaLang.ast.NumberUnit.Percentage

import scala.util.Success

object ColorOps {

	/**
		* A somewhat arbitrary value used for results featuring saturation or lightness
		*/
	final val PercentagePrecision = 3

	final val AlphaPrecision = 6


	private def getPresentableAlpha(color: Rgba): Double =
		NumberOps.normalizeDouble(color.a / 255d, AlphaPrecision)

	private def getPresentablePercentage(percentage: Double): Double =
		NumberOps.normalizeDouble(percentage, PercentagePrecision)

	/**
		*
		* @param h [0, 360)
		* @param s [0, 1)
		* @param l [0, 1)
		* @param a [0, 1)
		*/
	private case class Hsla(h: Double, s: Double, l: Double, a: Double)

	/**
		* Based very heavily on https://github.com/sksamuel/scrimage/blob/master/scrimage-core/src/main/scala/com/sksamuel/scrimage/color.scala
		*
		* @param color Rgba
		* @return Hsla
		*
		*/
	private def toHsla(color: Rgba): Hsla = {
		val r = color.r / 255d
		val g = color.g / 255d
		val b = color.b / 255d

		val max = r.max(g).max(b)
		val min = r.min(g).min(b)

		val l = (max + min) / 2d

		val (h: Double, s: Double) = if (min == max) {
			(0d, 0d) // Achromatic
		} else {
			val d = max - min
			val s = if (l > 0.5) d / (2 - max - min) else d / (max + min)
			val h = max match {
				case `r` => (g - b) / d + (if (g < b) 6 else 0)
				case `g` => (b - r) / d + 2
				case `b` => (r - g) / d + 4
			}
			(h / 6, s)
		}
		Hsla(h * 360d, s, l, color.a / 255d)
	}

	/**
		* Based very heavily on https://github.com/sksamuel/scrimage/blob/master/scrimage-core/src/main/scala/com/sksamuel/scrimage/color.scala
		*
		* @param color Hsla
		* @return Rgba
		*/
	private def hslaToRgba(color: Hsla): Rgba = {
		// assumes h is in th range [0,1] not [0,360) so must convert
		val h = color.h / 360d

		if (color.s == 0) {
			// achromatic
			Rgba(
				Math.round(color.l * 255d).toInt,
				Math.round(color.l * 255d).toInt,
				Math.round(color.l * 255d).toInt,
				Math.round(color.a * 255d).toInt
			)
		} else {
			def hue2rgb(p: Double, q: Double, t: Double): Int = {
				val tPrime: Double = if (t < 0) t + 1d else if (t > 1d) t - 1d else t
				val rgb = if (tPrime < 1d / 6d) p + (q - p) * 6d * tPrime
				else if (tPrime < 1d / 2d) q
				else if (tPrime < 2d / 3d) p + (q - p) * (2d / 3d - tPrime) * 6d
				else p

				Math.round(rgb * 255d).toInt
			}

			val q = if (color.l < 0.5d) color.l * (1d + color.s) else color.l + color.s - color.l * color.s
			val p = 2d * color.l - q

			Rgba(
				hue2rgb(p, q, h + 1d / 3d),
				hue2rgb(p, q, h),
				hue2rgb(p, q, h - 1d / 3d),
				Math.round(color.a * 255d).toInt
			)
		}
	}

	private def combineColors(x: Rgba, y: Rgba, combine: (Int, Int) => Int, normalize: (Int) => Int): Rgba =
		Rgba(
			normalize(combine(x.r, y.r)),
			normalize(combine(x.g, y.g)),
			normalize(combine(x.b, y.b)),
			normalize(combine(x.a, y.a))
		)

	/**
		* @param color  Input color
		* @param amount It's really a percentage point
		* @return
		*/
	private def adjustLightness(color: Rgba, amount: Value.Number, combine: (Double, Double) => Double, normalize: (Double) => Double): Rgba = {
		val hsla = toHsla(color)

		hslaToRgba(Hsla(
			hsla.h, hsla.s, normalize(combine(hsla.l, amount.value / 100d)), hsla.a
		))
	}


	def addColors(x: Rgba, y: Rgba): Rgba = combineColors(x, y, _ + _, _ min 255)

	def subtractColors(x: Rgba, y: Rgba): Rgba = combineColors(x, y, _ - _, _ max 0)

	def lighten(color: Rgba, amount: Value.Number): Rgba = adjustLightness(color, amount, _ + _, _ min 1)

	def darken(color: Rgba, amount: Value.Number): Rgba = adjustLightness(color, amount, _ - _, _ max 0)


	// Properties

	def alpha(color: Rgba): Value.Number =
		Value.Number(getPresentableAlpha(color))

	def complement(color: Rgba): Rgba = {
		val hsla = toHsla(color)
		val complement = hsla.copy(h = (hsla.h + 180) % 360)
		hslaToRgba(complement)
	}

	def hue(color: Rgba): Value.Number =
		Value.Number(toHsla(color).h.round)

	def inverted(color: Rgba): Value.Rgba =
		color.copy(r = 255 - color.r, g = 255 - color.g, b = 255 - color.b) // Preserving alpha

	def isDark(color: Rgba): Value.Boolean =
		Value.Boolean(toHsla(color).l < 0.5)

	def isLight(color: Rgba): Value.Boolean =
		Value.Boolean(toHsla(color).l >= 0.5)

	def lightness(color: Rgba): Value.Number =
		Value.Number(getPresentablePercentage(toHsla(color).l * 100), Percentage)

	def saturation(color: Rgba): Value.Number =
		Value.Number(getPresentablePercentage(toHsla(color).s * 100), Percentage)

	def toString(color: Rgba): Value.String = {
		if (color.a == 255) {
			val (r, g, b) = (color.r.toHexString, color.g.toHexString, color.b.toHexString)

			Value.String("#" + (
				if (color.r % 17 == 0 && color.g % 17 == 0 && color.b % 17 == 0) {
					s"${r.charAt(0)}${g.charAt(0)}${b.charAt(0)}"
				} else {
					s"${r.padTo(2, '0')}${g.padTo(2, '0')}${b.padTo(2, '0')}"
				}
			))
		} else
			Value.String(s"rgba(${color.r}, ${color.g}, ${color.b}, ${getPresentableAlpha(color)})")
	}


	// Method generators

	def getDarken(color: Rgba): Native =
		Native(Vector(Type.Percentage), (arguments: Vector[Value]) => {
			val delta = arguments(0).asInstanceOf[Value.Number]

			Success(darken(color, delta))
		})

	def getLighten(color: Rgba): Native =
		Native(Vector(Type.Percentage), (arguments: Vector[Value]) => {
			val delta = arguments(0).asInstanceOf[Value.Number]

			Success(lighten(color, delta))
		})

}
