package com.preprocessor.spec.types

import com.preprocessor.ast.Symbol

trait Type {

	def name: String

	def apply(): Symbol.TypeSymbol#Value

}
