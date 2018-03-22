package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Term.{FunctionCall, MemberAccess, Term}
import com.preprocessor.ast.Language.Value
import com.preprocessor.error.NativeError

class MemberAccessInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Member access interpreter"

	it should "interpret common number members" in {
		assert(
			run(MemberAccess(Value.Scalar(12), Value.String("isEven"))).value === Value.Boolean(true)
		)
		assert(
			run(MemberAccess(Value.Scalar(123), Value.String("isNegative"))).value === Value.Boolean(false)
		)
		assert(
			run(MemberAccess(Value.Scalar(-123), Value.String("isOdd"))).value === Value.Boolean(true)
		)
		assert(
			run(MemberAccess(Value.Scalar(-123), Value.String("isPositive"))).value === Value.Boolean(false)
		)
		assert(
			run(MemberAccess(Value.Scalar(-123.4), Value.String("isWhole"))).value === Value.Boolean(false)
		)
		assert(
			run(MemberAccess(Value.Scalar(-123), Value.String("toPercentage"))).value === Value.Percentage(-123)
		)
	}

	it should "interpret string members" in {
		assert(
			run(MemberAccess(Value.String("abc"), Value.String("length"))).value === Value.Scalar(3)
		)
		assert(
			run(MemberAccess(Value.String("ABC"), Value.String("toLowerCase"))).value === Value.String("abc")
		)
		assert(
			run(MemberAccess(Value.String("abc"), Value.String("toUpperCase"))).value === Value.String("ABC")
		)
		assert(
			run(MemberAccess(Value.String("abc"), Value.String("toString"))).value === Value.String("abc")
		)
		assert(
			run(MemberAccess(Value.String("  	  	abc		 "), Value.String("trim"))).value === Value.String("abc")
		)
	}

	it should "interpret string.charAt" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("abc"), Value.String("charAt")),
				Vector(Value.Scalar(1)))
			).value === Value.String("b")
		)
		assertThrows[NativeError](run(FunctionCall(
			MemberAccess(Value.String("abc"), Value.String("charAt")),
			Vector(Value.Scalar(-1)))
		))
		assertThrows[NativeError](run(FunctionCall(
			MemberAccess(Value.String("abc"), Value.String("charAt")),
			Vector(Value.Scalar(3)))
		))
	}

	it should "interpret string.concat" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("abc"), Value.String("concat")),
				Vector(Value.String("def")))
			).value === Value.String("abcdef")
		)
	}

	it should "interpret string.endsWith" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("abc"), Value.String("endsWith")),
				Vector(Value.String("bc")))
			).value === Value.Boolean(true)
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("abc"), Value.String("endsWith")),
				Vector(Value.String("")))
			).value === Value.Boolean(true)
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("abc"), Value.String("endsWith")),
				Vector(Value.String("abcdef")))
			).value === Value.Boolean(false)
		)
	}

	it should "interpret color members" in {
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("alpha"))).value === Value.Scalar(0.482353)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("blue"))).value === Value.Scalar(199)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199), Value.String("complement"))).value === Value.Rgba(12, 253, 66)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("green"))).value === Value.Scalar(12)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("hue"))).value === Value.Scalar(313)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("isDark"))).value === Value.Boolean(false)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("isLight"))).value === Value.Boolean(true)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("lightness"))).value === Value.Percentage(51.961)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("red"))).value === Value.Scalar(253)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("saturation"))).value === Value.Percentage(98.367)
		)
	}

	it should "interpret color.{darken, lighten}" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.Rgba(180, 212, 85), Value.String("darken")),
				Vector(Value.Percentage(15)))
			).value === Value.Rgba(143, 176, 45)
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.Rgba(240, 70, 21), Value.String("lighten")),
				Vector(Value.Percentage(25)))
			).value === Value.Rgba(248, 165, 141)
		)
	}

	protected def run(term: Term)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Term](TermInterpreter.run(_), term)

}
