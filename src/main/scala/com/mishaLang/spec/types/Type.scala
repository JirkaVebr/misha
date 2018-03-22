package com.mishaLang.spec.types

import com.mishaLang.interpreter.Symbol

trait Type {

	def name: String

	def apply(): Symbol.TypeSymbol#Value

}
