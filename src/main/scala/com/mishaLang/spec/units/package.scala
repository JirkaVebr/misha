package com.mishaLang.spec

package object units {

	lazy final val UnitsMap: Map[String, AtomicUnit] =
		Angle.AngleUnits ++
		Flex.FlexUnits ++
		Frequency.FrequencyUnits ++
		Length.LengthUnits ++
		Resolution.ResolutionUnits ++
		Time.TimeUnits
}
