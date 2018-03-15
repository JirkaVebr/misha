package com.preprocessor.spec

import com.preprocessor.ast.Symbol

package object types {

	val preDefinedTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		BlendMode.name -> BlendMode()
	)

}
