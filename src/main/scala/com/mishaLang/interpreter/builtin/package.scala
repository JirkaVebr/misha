package com.mishaLang.interpreter

package object builtin {


	lazy final val Builtins: Map[String, Symbol.ValueSymbol#Value] =
		Trigonometry.Trigonometry ++
		Constants.Constants

}
