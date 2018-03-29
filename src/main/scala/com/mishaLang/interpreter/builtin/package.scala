package com.mishaLang.interpreter

import com.mishaLang.interpreter.Symbol.ValueSymbol

package object builtin {


	lazy final val Builtins: Map[ValueSymbol, Symbol.ValueSymbol#Value] =
		Trigonometry.Trigonometry ++
		Constants.Constants

}
