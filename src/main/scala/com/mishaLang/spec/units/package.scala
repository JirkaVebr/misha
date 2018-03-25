package com.mishaLang.spec

package object units {

	lazy val unitsMap: Map[String, AtomicUnit] =
		Angle.angleUnits ++
		Flex.flexUnits ++
		Frequency.frequencyUnits ++
		Length.lengthUnits ++
		Resolution.resolutionUnits ++
		Time.timeUnits
}
