package com.preprocessor

package object ast {

	@inline implicit def string2CssIdentifier(string: String): CssIdentifier = CssIdentifier(string)
	@inline implicit def cssIdentifier2String(identifier: CssIdentifier): String = identifier.value

}
