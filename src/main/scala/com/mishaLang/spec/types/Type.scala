package com.mishaLang.spec.types

import com.mishaLang.interpreter.Symbol
import com.mishaLang.interpreter.Symbol.TypeSymbol

trait Type {

	def name: TypeSymbol

	def apply(): Symbol.TypeSymbol#Value

}
