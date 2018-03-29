package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.Number
import com.mishaLang.interpreter.BaseInterpreterSpec

class ListOpsSpec extends BaseInterpreterSpec {

	behavior of "List operations"

	it should "correctly repeat lists" in {
		assert(ListOps.repeat(Value.List(Vector(
			Number(1), Number(2), Number(3)
		)), 3) === Value.List(Vector(
			Number(1), Number(2), Number(3), Number(1), Number(2), Number(3), Number(1), Number(2), Number(3)
		)))
		assert(ListOps.repeat(Value.List(Vector()), 3) === Value.List(Vector()))
		assert(ListOps.repeat(Value.List(Vector()), -3) === Value.List(Vector()))
	}

	it should "correctly append to lists" in {
		assert(ListOps.append(Value.List(Vector(
			Number(1), Number(2)
		)), Number(3)) === Value.List(Vector(Number(1), Number(2), Number(3))))
	}

	it should "correctly prepend to lists" in {
		assert(ListOps.prepend(Value.List(Vector(
			Number(1), Number(2)
		)), Number(3)) === Value.List(Vector(Number(3), Number(1), Number(2))))
	}

}
