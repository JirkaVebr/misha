package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.{Type, Value}
import com.preprocessor.ast.Ast.Type.Color
import com.preprocessor.ast.RuleContext.Selector.Universal
import com.preprocessor.ast.Symbol.{Context, TypeSymbol, ValueSymbol}
import com.preprocessor.ast.ValueRecord

class EnvironmentSpec extends BaseInterpreterSpec {

	behavior of "Environment"

	it should "behave correctly when empty" in {
		val environment = new Environment
		val absentType = TypeSymbol("AbsentType")

		assert(environment.lookup(absentType).isEmpty)
		assert(!environment.isInCurrentScope(absentType))
		assert(!environment.isInScope(absentType))
	}

	it should "successfully put and retrieve data" in {
		var environment = new Environment

		val testTypeSymbol = TypeSymbol("testTypeSymbol")
		val testType = Color

		environment = environment.updated(testTypeSymbol)(testType)

		assert(environment.lookup(testTypeSymbol).nonEmpty)
		assert(environment.lookup(testTypeSymbol).get == testType)
		assert(environment.isInCurrentScope(testTypeSymbol))
		assert(environment.isInScope(testTypeSymbol))
	}

	it should "handle symbols of various types" in {
		var environment = new Environment

		val testTypeSymbol = TypeSymbol("test")
		val testType = Color
		val testValueSymbol = ValueSymbol("test")
		val testValue = ValueRecord(Value.Number(1))()
		val testContext = Universal

		assert(environment.lookup(testTypeSymbol).isEmpty)
		assert(environment.lookup(testValueSymbol).isEmpty)
		assert(environment.lookup(Context).isEmpty)

		environment = environment.updated(testTypeSymbol)(testType)
		environment = environment.updated(testValueSymbol)(testValue)
		environment = environment.updated(Context)(testContext)

		assert(environment.lookup(testTypeSymbol).nonEmpty)
		assert(environment.lookup(testValueSymbol).nonEmpty)
		assert(environment.lookup(Context).nonEmpty)

		assert(environment.lookup(testTypeSymbol).get == testType)
		assert(environment.lookup(testValueSymbol).get == testValue)
		assert(environment.lookup(Context).get == testContext)
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
		assert(childEnvironment.isInScope(testTypeSymbol))

		val parentEnvironmentAgain = childEnvironment.popSubScope()
		assert(parentEnvironmentAgain.get.lookup(testTypeSymbol).get == testTypeParent)
	}
}
