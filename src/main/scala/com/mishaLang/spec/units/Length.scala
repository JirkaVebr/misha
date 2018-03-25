package com.mishaLang.spec.units

object Length {

	sealed trait Length extends AtomicUnit


	sealed trait Absolute extends Length
	case object Pixel extends Absolute { override val symbol = "px" }
	case object CentiMeter extends Absolute { override val symbol = "cm" }
	case object MiliMeter extends Absolute { override val symbol = "mm" }
	case object QuarterMiliMeter extends Absolute { override val symbol = "Q" }
	case object Inch extends Absolute { override val symbol = "in" }
	case object Pica extends Absolute { override val symbol = "pc" }
	case object Point extends Absolute { override val symbol = "pt" }


	sealed trait FontRelative extends Length
	case object CapitalHeight extends FontRelative { override val symbol = "cap" }
	case object AdvanceMeasureZero extends FontRelative { override val symbol = "ch" }
	case object Em extends FontRelative { override val symbol = "em" }
	case object XHeight extends FontRelative { override val symbol = "ex" }
	case object AdvanceMeasureCJK extends FontRelative { override val symbol = "ic" }
	case object LineHeight extends FontRelative { override val symbol = "lh" }
	case object RootEm extends FontRelative { override val symbol = "rem" }
	case object RootLineHeight extends FontRelative { override val symbol = "rlh" }


	sealed trait ViewportRelative extends Length
	case object ViewportHeight extends ViewportRelative { override val symbol = "vh" }
	case object ViewportWidth extends ViewportRelative { override val symbol = "vw" }
	case object ViewportInline extends ViewportRelative { override val symbol = "vi" }
	case object ViewportBlock extends ViewportRelative { override val symbol = "vb" }
	case object ViewportMin extends ViewportRelative { override val symbol = "vmin" }
	case object ViewportMax extends ViewportRelative { override val symbol = "vmax" }


	lazy val lengthUnits: Map[String, Length] = Map(
		AdvanceMeasureCJK.symbol -> AdvanceMeasureCJK,
		AdvanceMeasureZero.symbol -> AdvanceMeasureZero,
		CapitalHeight.symbol -> CapitalHeight,
		CentiMeter.symbol -> CentiMeter,
		Em.symbol -> Em,
		Inch.symbol -> Inch,
		LineHeight.symbol -> LineHeight,
		MiliMeter.symbol -> MiliMeter,
		Pica.symbol -> Pica,
		Pixel.symbol -> Pixel,
		Point.symbol -> Point,
		QuarterMiliMeter.symbol -> QuarterMiliMeter,
		RootEm.symbol -> RootEm,
		RootLineHeight.symbol -> RootLineHeight,
		ViewportBlock.symbol -> ViewportBlock,
		ViewportHeight.symbol -> ViewportHeight,
		ViewportInline.symbol -> ViewportInline,
		ViewportMax.symbol -> ViewportMax,
		ViewportMin.symbol -> ViewportMin,
		ViewportWidth.symbol -> ViewportWidth,
		XHeight.symbol -> XHeight
	)

}
