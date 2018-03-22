package com.mishaLang.spec

import com.mishaLang.interpreter.Symbol

package object types {

	val specTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		Animatable.name -> Animatable(),
		BlendMode.name -> BlendMode()
	)

}
