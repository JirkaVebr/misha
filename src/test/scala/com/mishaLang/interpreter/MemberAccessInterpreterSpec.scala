package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Term.{FunctionCall, MemberAccess, Term}
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.NumberUnit.Percentage
import com.mishaLang.error.NativeError

class MemberAccessInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Member access interpreter"

	it should "interpret common number members" in {
		assert(
			run(MemberAccess(Value.Number(12), Value.String("isEven"))).value === Value.Boolean(true)
		)
		assert(
			run(MemberAccess(Value.Number(123), Value.String("isNegative"))).value === Value.Boolean(false)
		)
		assert(
			run(MemberAccess(Value.Number(-123), Value.String("isOdd"))).value === Value.Boolean(true)
		)
		assert(
			run(MemberAccess(Value.Number(-123), Value.String("isPositive"))).value === Value.Boolean(false)
		)
		assert(
			run(MemberAccess(Value.Number(-123.4), Value.String("isWhole"))).value === Value.Boolean(false)
		)
		assert(
			run(MemberAccess(Value.Number(25), Value.String("sqrt"))).value === Value.Number(5)
		)
		assert(
			run(MemberAccess(Value.Number(-123), Value.String("toPercentage"))).value === Value.Number(-123, Percentage)
		)
	}

	it should "interpret string members" in {
		assert(
			run(MemberAccess(Value.String("abc"), Value.String("length"))).value === Value.Number(3)
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
				Vector(Value.Number(1)))
			).value === Value.String("b")
		)
		assertThrows[NativeError](run(FunctionCall(
			MemberAccess(Value.String("abc"), Value.String("charAt")),
			Vector(Value.Number(-1)))
		))
		assertThrows[NativeError](run(FunctionCall(
			MemberAccess(Value.String("abc"), Value.String("charAt")),
			Vector(Value.Number(3)))
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


	it should "interpret string.split" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("a,b,third"), Value.String("split")),
				Vector(Value.String(","))
			)).value === Value.List(Vector(Value.String("a"), Value.String("b"), Value.String("third")))
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("a,b,"), Value.String("split")),
				Vector(Value.String(","))
			)).value === Value.List(Vector(Value.String("a"), Value.String("b"), Value.String("")))
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("a|b|"), Value.String("split")),
				Vector(Value.String("|"))
			)).value === Value.List(Vector(Value.String("a"), Value.String("b"), Value.String("")))
		)
	}

	it should "interpret string.replace" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("hello"), Value.String("replace")),
				Vector(Value.String("ll"), Value.String("llll"))
			)).value === Value.String("hellllo")
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("(foo)?(@)+"), Value.String("replace")),
				Vector(Value.String(")?"), Value.String(":)"))
			)).value === Value.String("(foo:)(@)+")
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("ololololo"), Value.String("replace")),
				Vector(Value.String("(ol)+"), Value.String("hello"))
			)).value === Value.String("ololololo")
		)
	}

	it should "interpret string.startsWith" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("mishalang"), Value.String("startsWith")),
				Vector(Value.String("misha"))
			)).value === Value.Boolean(true)
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("Mishalang"), Value.String("startsWith")),
				Vector(Value.String("misha"))
			)).value === Value.Boolean(false)
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("mishalang"), Value.String("startsWith")),
				Vector(Value.String("(mi)?"))
			)).value === Value.Boolean(false)
		)
	}

	it should "interpret string.indexOf" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("foobar"), Value.String("indexOf")),
				Vector(Value.String("o"))
			)).value === Value.Number(1)
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("foobar"), Value.String("indexOf")),
				Vector(Value.String("ba"))
			)).value === Value.Number(3)
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("alpha"), Value.String("indexOf")),
				Vector(Value.String("beta"))
			)).value === Value.Number(-1)
		)
	}

	it should "interpret string.repeat" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("Mi"), Value.String("repeat")),
				Vector(Value.Number(3))
			)).value === Value.String("MiMiMi")
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("zero"), Value.String("repeat")),
				Vector(Value.Number(0))
			)).value === Value.String("")
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("minus one"), Value.String("repeat")),
				Vector(Value.Number(-1))
			)).value === Value.String("")
		)
	}

	it should "interpret string.substring" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("mishalang"), Value.String("substring")),
				Vector(Value.Number(5))
			)).value === Value.String("lang")
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.String("mishalang"), Value.String("substring")),
				Vector(Value.Number(5), Value.Number(7))
			)).value === Value.String("la")
		)
	}



	it should "interpret color members" in {
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("alpha"))).value === Value.Number(0.482353)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("blue"))).value === Value.Number(199)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199), Value.String("complement"))).value === Value.Rgba(12, 253, 66)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("green"))).value === Value.Number(12)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("hue"))).value === Value.Number(313)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("isDark"))).value === Value.Boolean(false)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("isLight"))).value === Value.Boolean(true)
		)
		assert(
			run(MemberAccess(Value.Rgba(214, 40, 100, 123), Value.String("inverted"))).value === Value.Rgba(41, 215, 155, 123)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("lightness"))).value === Value.Number(51.961, Percentage)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("red"))).value === Value.Number(253)
		)
		assert(
			run(MemberAccess(Value.Rgba(253, 12, 199, 123), Value.String("saturation"))).value === Value.Number(98.367, Percentage)
		)
	}

	it should "interpret color.{darken, lighten}" in {
		assert(
			run(FunctionCall(
				MemberAccess(Value.Rgba(180, 212, 85), Value.String("darken")),
				Vector(Value.Number(15, Percentage)))
			).value === Value.Rgba(143, 176, 45)
		)
		assert(
			run(FunctionCall(
				MemberAccess(Value.Rgba(240, 70, 21), Value.String("lighten")),
				Vector(Value.Number(25, Percentage)))
			).value === Value.Rgba(248, 165, 141)
		)
	}

	protected def run(term: Term)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Term](TermInterpreter.run(_)(state), term)

}
