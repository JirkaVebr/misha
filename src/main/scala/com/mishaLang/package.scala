package com

import com.mishaLang.interpreter.Symbol.{TypeSymbol, ValueSymbol}

package object mishaLang {

	@inline implicit def string2TypeSymbol(string: String): TypeSymbol = TypeSymbol(string)
	@inline implicit def typeSymbol2String(symbol: TypeSymbol): String = symbol.typeName

	@inline implicit def string2ValueSymbol(string: String): ValueSymbol = ValueSymbol(string)
	@inline implicit def valueSymbol2String(symbol: ValueSymbol): String = symbol.valueName
}
