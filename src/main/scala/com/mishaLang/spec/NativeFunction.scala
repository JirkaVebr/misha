package com.mishaLang.spec

object NativeFunction {

	sealed trait NativeFunction {
		def name: String
	}

}
