package com.mishaLang.interpreter.builtin

import com.mishaLang.ast.Language.Term.Variable
import com.mishaLang.ast.Language.Value

class ConstantsSpec extends BaseBuiltinSpec {

	behavior of "Constants"


	they should "evaluate as expected" in {
		assert(run(Variable("E")).value.asInstanceOf[Value.Number].value === Math.E)
		assert(run(Variable("PI")).value.asInstanceOf[Value.Number].value === Math.PI)
		assert(run(Variable("TAU")).value.asInstanceOf[Value.Number].value === 2 * Math.PI)
		assert(run(Variable("GOLDEN_RATIO")).value.asInstanceOf[Value.Number].value === (1 + Math.sqrt(5)) / 2)
	}
}
