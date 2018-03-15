package com.preprocessor.spec

import com.preprocessor.ast.Symbol

package object types {

	val specTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		BlendMode.name -> BlendMode()
	)

}
