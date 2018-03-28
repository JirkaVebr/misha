package com.mishaLang.spec

import com.mishaLang.interpreter.Symbol

package object types {

	lazy final val SpecTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		Animatable.name -> Animatable(),
		BlendMode.name -> BlendMode(),
		Dimension.name -> Dimension(),
		DimensionLimit.name -> DimensionLimit(),
		Global.name -> Global(),
		Margin.name -> Margin(),
		Overflow.name -> Overflow(),
		Padding.name -> Padding(),
		Visibility.name -> Visibility(),
	)

}
