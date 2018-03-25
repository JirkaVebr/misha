package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value

object ListOps {

	def append(list: Value.List, newItem: Value.Value): Value.List =
		Value.List(list.values :+ newItem)


	def repeat(list: Value.List, factor: Int): Value.List = {
		def appendNTimes[T](list: Vector[T], n: Int): Vector[T] =
			if (n == 0) Vector.empty[T]
			else appendNTimes(list, n - 1) ++ list
		Value.List(appendNTimes(list.values, if (factor >= 0) factor else 0))
	}


	// Properties

	def length(list: Value.List): Value.Scalar =
		Value.Scalar(list.values.length)

}
