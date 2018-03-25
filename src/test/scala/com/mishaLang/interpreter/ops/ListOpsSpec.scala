package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.Scalar
import com.mishaLang.interpreter.BaseInterpreterSpec

class ListOpsSpec extends BaseInterpreterSpec {

	behavior of "List operations"

	it should "correctly repeat lists" in {
		assert(ListOps.repeat(Value.List(Vector(
			Scalar(1), Scalar(2), Scalar(3)
		)), 3) === Value.List(Vector(
			Scalar(1), Scalar(2), Scalar(3), Scalar(1), Scalar(2), Scalar(3), Scalar(1), Scalar(2), Scalar(3)
		)))
		assert(ListOps.repeat(Value.List(Vector()), 3) === Value.List(Vector()))
		assert(ListOps.repeat(Value.List(Vector()), -3) === Value.List(Vector()))
	}

	it should "correctly append to lists" in {
		assert(ListOps.append(Value.List(Vector(
			Scalar(1), Scalar(2)
		)), Scalar(3)) === Value.List(Vector(Scalar(1), Scalar(2), Scalar(3))))
	}

	it should "correctly prepend to lists" in {
		assert(ListOps.prepend(Value.List(Vector(
			Scalar(1), Scalar(2)
		)), Scalar(3)) === Value.List(Vector(Scalar(3), Scalar(1), Scalar(2))))
	}

}
