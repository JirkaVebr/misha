package com.mishaLang.spec

import com.mishaLang.interpreter.Symbol

package object types {

	lazy final val SpecTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		Animatable.name -> Animatable(),
		BlendMode.name -> BlendMode(),
		Global.name -> Global(),
		Height.name -> Height(),
		Margin.name -> Margin(),
		Overflow.name -> Overflow(),
		Padding.name -> Padding(),
		Visibility.name -> Visibility(),
		Width.name -> Width(),
	)

}
