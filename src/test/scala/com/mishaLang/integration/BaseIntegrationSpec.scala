package com.mishaLang.integration

import com.mishaLang
import com.mishaLang.BaseSpec

class BaseIntegrationSpec extends BaseSpec {

	protected def run(input: String): String =
		mishaLang.Compiler.compileString(input)
}
