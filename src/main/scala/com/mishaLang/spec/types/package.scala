package com.mishaLang.spec

import com.mishaLang.interpreter.Symbol

package object types {

	lazy final val SpecTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		Animatable.name -> Animatable(),
		BlendMode.name -> BlendMode(),
		Global.name -> Global(),
		Margin.name -> Margin(),
		Padding.name -> Padding(),
		Width.name -> Width(),
	)

}
