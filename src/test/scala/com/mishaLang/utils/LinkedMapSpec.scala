package com.mishaLang.utils

class LinkedMapSpec extends BaseUtilsSpec {

	behavior of "LinkedMap"

	val map: LinkedMap[Int, String] = LinkedMap.empty

	it should "behave as expected when empty" in {
		assert(!map.contains(123))
		assert(map.size === 0)
	}

	it should "preserve key insertion order" in {
		val updated = map
			.updated(123, "123")
			.updated(456, "456")
			.updated(100, "100")

		assert(updated.toList === List(
			(123, "123"),
			(456, "456"),
			(100, "100")
		))
	}

	it should "preserve key insertion order when dealing with duplicates" in {
		val updated = map
			.updated(123, "123")
			.updated(456, "456")
			.updated(123, "123 again!")
			.updated(100, "100")

		assert(updated.toList === List(
			(123, "123 again!"),
			(456, "456"),
			(100, "100")
		))
	}

}
