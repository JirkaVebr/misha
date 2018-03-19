package com.preprocessor.spec

object NativeFunction {

	sealed trait NativeFunction {
		def name: String
	}

}
