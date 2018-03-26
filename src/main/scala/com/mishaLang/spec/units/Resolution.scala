package com.mishaLang.spec.units

object Resolution {

	sealed abstract class Resolution extends AtomicUnit
	case object DotsPerInch extends Resolution { override val symbol = "dpi" }
	case object DotsPerCentiMeter extends Resolution { override val symbol = "dpcm" }
	case object DotsPerPixel extends Resolution { override val symbol = "dppx" }

	lazy final val ResolutionUnits: Map[String, Resolution] = Map(
		DotsPerCentiMeter.symbol -> DotsPerCentiMeter,
		DotsPerInch.symbol -> DotsPerInch,
		DotsPerPixel.symbol -> DotsPerPixel
	)

}
