package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value

object ListOps {

	def append(list: Value.List, newItem: Value.Value): Value.List =
		Value.List(list.values :+ newItem)

	def prepend(list: Value.List, newItem: Value.Value): Value.List =
		Value.List(newItem +: list.values)

	def concatenate(left: Value.List, right: Value.List): Value.List =
		Value.List(left.values ++ right.values)


	def repeat(list: Value.List, factor: Int): Value.List = {
		def appendNTimes[T](list: Vector[T], n: Int): Vector[T] =
			if (n == 0) Vector.empty[T]
			else appendNTimes(list, n - 1) ++ list
		Value.List(appendNTimes(list.values, if (factor >= 0) factor else 0))
	}


	// Properties

	def length(list: Value.List): Value.Number =
		Value.Number(list.values.length)

}
