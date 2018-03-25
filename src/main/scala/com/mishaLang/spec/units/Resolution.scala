package com.mishaLang.spec.units

object Resolution {

	sealed trait Resolution extends AtomicUnit
	case object DotsPerInch extends Resolution { override val symbol = "dpi" }
	case object DotsPerCentiMeter extends Resolution { override val symbol = "dpcm" }
	case object DotsPerPixel extends Resolution { override val symbol = "dppx" }

	lazy val resolutionUnits: Map[String, Resolution] = Map(
		DotsPerCentiMeter.symbol -> DotsPerCentiMeter,
		DotsPerInch.symbol -> DotsPerInch,
		DotsPerPixel.symbol -> DotsPerPixel
	)

}
