package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.{Type, Value}
import com.preprocessor.ast.Ast.Type.Color
import com.preprocessor.ast.Selector.Universal
import com.preprocessor.ast.Symbol.{Selector, TypeSymbol, ValueSymbol}

class EnvironmentSpec extends BaseInterpreterSpec {

	behavior of "Environment"

	it should "behave correctly when empty" in {
		val environment = new Environment

		assert(environment.lookup(TypeSymbol("AbsentType")).isEmpty)
	}

	it should "successfully put and retrieve data" in {
		var environment = new Environment

		val testTypeSymbol = TypeSymbol("testTypeSymbol")
		val testType = Color

		assert(environment.lookup(testTypeSymbol).isEmpty)

		environment = environment.updated(testTypeSymbol)(testType)

		assert(environment.lookup(testTypeSymbol).nonEmpty)
		assert(environment.lookup(testTypeSymbol).get == testType)
	}

	it should "handle symbols of various types" in {
		var environment = new Environment

		val testTypeSymbol = TypeSymbol("test")
		val testType = Color
		val testValueSymbol = ValueSymbol("test")
		val testValue = Value.Number(1)
		val testSelector = Universal

		assert(environment.lookup(testTypeSymbol).isEmpty)
		assert(environment.lookup(testValueSymbol).isEmpty)
		assert(environment.lookup(Selector).isEmpty)

		environment = environment.updated(testTypeSymbol)(testType)
		environment = environment.updated(testValueSymbol)(testValue)
		environment = environment.updated(Selector)(testSelector)

		assert(environment.lookup(testTypeSymbol).nonEmpty)
		assert(environment.lookup(testValueSymbol).nonEmpty)
		assert(environment.lookup(Selector).nonEmpty)

		assert(environment.lookup(testTypeSymbol).get == testType)
		assert(environment.lookup(testValueSymbol).get == testValue)
		assert(environment.lookup(Selector).get == testSelector)
	}

	it should "retrieve data from parent" in {
		var parentEnvironment = new Environment

		val testTypeSymbol = TypeSymbol("testTypeSymbol")
		val testType = Color

		assert(parentEnvironment.lookup(testTypeSymbol).isEmpty)

		parentEnvironment = parentEnvironment.updated(testTypeSymbol)(testType)

		val childEnvironment = parentEnvironment.pushSubScope()

		assert(childEnvironment.lookup(testTypeSymbol).nonEmpty)
		assert(childEnvironment.lookup(testTypeSymbol).get == testType)
		assert(childEnvironment.lookup(TypeSymbol("AbsentTypeName")).isEmpty)
	}

	it should "shadow data from parent" in {
		var parentEnvironment = new Environment

		val testTypeSymbol = TypeSymbol("testTypeSymbol")
		val testTypeParent = Color
		val testTypeChild = Type.Number

		assert(parentEnvironment.lookup(testTypeSymbol).isEmpty)

		parentEnvironment = parentEnvironment.updated(testTypeSymbol)(testTypeParent)

		var childEnvironment = parentEnvironment.pushSubScope()
		childEnvironment = childEnvironment.updated(testTypeSymbol)(testTypeChild)

		assert(childEnvironment.lookup(testTypeSymbol).nonEmpty)
		assert(childEnvironment.lookup(testTypeSymbol).get == testTypeChild)
		assert(parentEnvironment.lookup(testTypeSymbol).get == testTypeParent)
	}
}
