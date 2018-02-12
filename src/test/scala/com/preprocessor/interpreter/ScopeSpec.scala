package com.preprocessor.interpreter

import com.preprocessor.TypeScope
import com.preprocessor.ast.Ast.Type
import com.preprocessor.ast.Ast.Type.Color
import com.preprocessor.ast.Symbol.TypeSymbol

class ScopeSpec extends BaseInterpreterSpec {

	behavior of "Scope"

	it should "behave correctly when empty" in {
		val typeScope = new TypeScope

		assert(typeScope.lookup(TypeSymbol("AbsentType")).isEmpty)
	}

	it should "successfully put and retrieve data" in {
		var typeScope = new TypeScope

		val testTypeName = "TestTypeName"
		val testType = Color

		assert(typeScope.lookup(testTypeName).isEmpty)

		typeScope = typeScope.updated(testTypeName, testType)

		assert(typeScope.lookup(testTypeName).nonEmpty)
		assert(typeScope.lookup(testTypeName).get == testType)
	}

	it should "retrieve data from parent" in {
		var parentTypeScope = new TypeScope

		val testTypeName = "TestTypeName"
		val testType = Color

		assert(parentTypeScope.lookup(testTypeName).isEmpty)

		parentTypeScope = parentTypeScope.updated(testTypeName, testType)

		val childScope = parentTypeScope.pushSubScope()

		assert(childScope.lookup(testTypeName).nonEmpty)
		assert(childScope.lookup(testTypeName).get == testType)
		assert(childScope.lookup("AbsentTypeName").isEmpty)
	}

	it should "shadow data from parent" in {
		var parentTypeScope = new TypeScope

		val testTypeName = "TestTypeName"
		val testTypeParent = Color
		val testTypeChild = Type.Number

		assert(parentTypeScope.lookup(testTypeName).isEmpty)

		parentTypeScope = parentTypeScope.updated(testTypeName, testTypeParent)

		var childScope = parentTypeScope.pushSubScope()
		childScope = childScope.updated(testTypeName, testTypeChild)

		assert(childScope.lookup(testTypeName).nonEmpty)
		assert(childScope.lookup(testTypeName).get == testTypeChild)
		assert(parentTypeScope.lookup(testTypeName).get == testTypeParent)
	}
}
