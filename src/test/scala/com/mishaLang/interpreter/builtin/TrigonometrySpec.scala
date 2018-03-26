package com.mishaLang.interpreter.builtin

import com.mishaLang.ast.Language.Expression.Expression
import com.mishaLang.ast.Language.Term.{FunctionCall, Variable}
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.NumberUnit.Atomic
import com.mishaLang.interpreter.{BaseInterpreterSpec, EnvWithValue, ExpressionInterpreter}
import com.mishaLang.spec.units.Angle.{Degree, Gradian, Radian, Turn}
import org.scalactic.{Equality, TolerantNumerics}

class TrigonometrySpec extends BaseInterpreterSpec {

	implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.00000001)

	behavior of "Trigonometric builtins"


	it should "correctly evaluate the sin function" in {
		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Scalar(Math.PI / 2))
		)).value === Value.Scalar(1))

		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Dimensioned(45, Atomic(Degree)))
		)).value.asInstanceOf[Value.Scalar].value === (Math.sqrt(2) / 2))

		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Dimensioned(Math.PI, Atomic(Radian)))
		)).value.asInstanceOf[Value.Scalar].value === 0d)

		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Dimensioned(.25, Atomic(Turn)))
		)).value === Value.Scalar(1))

		assert(run(FunctionCall(
			Variable("sin"), Vector(Value.Dimensioned(100, Atomic(Gradian)))
		)).value === Value.Scalar(1))
	}


	protected def run(expression: Expression)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Expression](ExpressionInterpreter.run(_)(state), expression)
}
