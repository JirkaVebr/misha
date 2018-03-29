package com.mishaLang.interpreter.builtin

import com.mishaLang.ast.Language.Term.{FunctionCall, Variable}
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.NumberUnit.Atomic
import com.mishaLang.spec.units.Angle.{Degree, Gradian, Radian, Turn}

class TrigonometrySpec extends BaseBuiltinSpec {

	behavior of "Trigonometric builtins"


	it should "correctly evaluate the sin function" in {
		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Number(Math.PI / 2))
		)).value === Value.Number(1))

		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Number(45, Atomic(Degree)))
		)).value.asInstanceOf[Value.Number].value === (Math.sqrt(2) / 2))

		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Number(Math.PI, Atomic(Radian)))
		)).value.asInstanceOf[Value.Number].value === 0d)

		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Number(.25, Atomic(Turn)))
		)).value === Value.Number(1))

		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Number(100, Atomic(Gradian)))
		)).value === Value.Number(1))
	}

	it should "correctly evaluate other trigonometric functions" in {
		assert(run(FunctionCall(
			Variable("cos"), Vector(Value.Number(45, Atomic(Degree)))
		)).value.asInstanceOf[Value.Number].value === (Math.sqrt(2) / 2))

		assert(run(FunctionCall(
			Variable("tan"), Vector(Value.Number(45, Atomic(Degree)))
		)).value.asInstanceOf[Value.Number].value === 1d)
	}

}
