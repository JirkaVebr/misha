package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Term.{FunctionCall, MemberAccess, Term}
import com.preprocessor.ast.Language.Value
import com.preprocessor.error.{NativeError, ProgramError}

class MemberAccessInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Member access interpreter"

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

	protected def run(term: Term)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Term](TermInterpreter.run(_), term)

}
