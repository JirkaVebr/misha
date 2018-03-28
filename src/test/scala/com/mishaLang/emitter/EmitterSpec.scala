package com.mishaLang.emitter

import com.mishaLang.interpreter.Environment

class EmitterSpec extends BaseEmitterSpec {

	behavior of "The emitter"

	// TODO

	def emit(environment: Environment): String =
		new Emitter(environment).emit().toString()
}
