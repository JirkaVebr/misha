package com.mishaLang.interpreter.builtin

import com.mishaLang.ast.Language.Expression.Expression
import com.mishaLang.interpreter.{BaseInterpreterSpec, EnvWithValue, ExpressionInterpreter}

class BaseBuiltinSpec extends BaseInterpreterSpec {


	protected def run(expression: Expression)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Expression](ExpressionInterpreter.run(_)(state), expression)
}
