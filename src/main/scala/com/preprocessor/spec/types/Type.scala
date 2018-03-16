package com.preprocessor.spec.types

import com.preprocessor.interpreter.Symbol

trait Type {

	def name: String

	def apply(): Symbol.TypeSymbol#Value

}
