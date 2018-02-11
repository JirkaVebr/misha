package com

import com.preprocessor.ast.Symbol.{TypeSymbol, ValueSymbol}
import com.preprocessor.interpreter.Scope

package object preprocessor {

	type TypeScope = Scope[TypeSymbol, TypeSymbol#Value]
	type ValueScope = Scope[ValueSymbol, ValueSymbol#Value]

	@inline implicit def string2TypeSymbol(string: String): TypeSymbol = TypeSymbol(string)
	@inline implicit def typeSymbol2String(symbol: TypeSymbol): String = symbol.underlying

	@inline implicit def string2ValueSymbol(string: String): ValueSymbol = ValueSymbol(string)
	@inline implicit def valueSymbol2String(symbol: ValueSymbol): String = symbol.underlying
}
