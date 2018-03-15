package com.preprocessor.spec

import com.preprocessor.ast.Symbol

package object types {

	val specTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		Animatable.name -> Animatable(),
		BlendMode.name -> BlendMode()
	)

}
