package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Term.MemberAccess
import com.preprocessor.ast.Language.Value

class MemberAccessInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Member access interpreter"

	it should "interpret string members" in {
		assert(
			run(MemberAccess(Value.String("abc"), Value.String("length"))).valueRecord.value === Value.Scalar(3)
		)
		assert(
			run(MemberAccess(Value.String("ABC"), Value.String("toLowerCase"))).valueRecord.value === Value.String("abc")
		)
		assert(
			run(MemberAccess(Value.String("abc"), Value.String("toUpperCase"))).valueRecord.value === Value.String("ABC")
		)
		assert(
			run(MemberAccess(Value.String("abc"), Value.String("toString"))).valueRecord.value === Value.String("abc")
		)
		assert(
			run(MemberAccess(Value.String("  	  	abc		 "), Value.String("trim"))).valueRecord.value === Value.String("abc")
		)
	}

	protected def run(memberAccess: MemberAccess)(implicit state: EvalState): EvalState =
		super.run[MemberAccess](MemberAccessInterpreter.run(_), memberAccess)

}
