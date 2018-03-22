package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Type.Color
import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.ast.Selector.Class
import com.mishaLang.interpreter.RuleContext.RuleSelector
import com.mishaLang.interpreter.Symbol.{RuleContextSymbol, TypeSymbol, ValueSymbol}

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

		environment = environment.putNew(testTypeSymbol)(testType)

		assert(environment.lookup(testTypeSymbol).nonEmpty)
		assert(environment.lookup(testTypeSymbol).get === testType)
		assert(environment.isInCurrentScope(testTypeSymbol))
		assert(environment.isInScope(testTypeSymbol))
	}

	it should "handle symbols of various types" in {
		var environment = new Environment

		val testTypeSymbol = TypeSymbol("test")
		val testType = Color
		val testValueSymbol = ValueSymbol("test")
		val testValue = Value.Scalar(1)
		val testContext = RuleSelector(Class("myClass"))

		assert(environment.lookup(testTypeSymbol).isEmpty)
		assert(environment.lookup(testValueSymbol).isEmpty)
		assert(environment.lookup(RuleContextSymbol).isEmpty)

		environment = environment.putNew(testTypeSymbol)(testType)
		environment = environment.putNew(testValueSymbol)(testValue)
		environment = environment.putNew(RuleContextSymbol)(testContext)

		assert(environment.lookup(testTypeSymbol).nonEmpty)
		assert(environment.lookup(testValueSymbol).nonEmpty)
		assert(environment.lookup(RuleContextSymbol).nonEmpty)

		assert(environment.lookup(testTypeSymbol).get === testType)
		assert(environment.lookup(testValueSymbol).get === testValue)
		assert(environment.lookup(RuleContextSymbol).get === testContext)
	}

	it should "retrieve data from parent" in {
		var parentEnvironment = new Environment

		val testTypeSymbol = TypeSymbol("testTypeSymbol")
		val testType = Color

		assert(parentEnvironment.lookup(testTypeSymbol).isEmpty)

		parentEnvironment = parentEnvironment.putNew(testTypeSymbol)(testType)

		val childEnvironment = parentEnvironment.pushSubScope()

		assert(childEnvironment.lookup(testTypeSymbol).nonEmpty)
		assert(childEnvironment.lookup(testTypeSymbol).get === testType)
		assert(childEnvironment.lookup(TypeSymbol("AbsentTypeName")).isEmpty)
	}

	it should "shadow data from parent" in {
		var parentEnvironment = new Environment

		val testTypeSymbol = TypeSymbol("testTypeSymbol")
		val testTypeParent = Color
		val testTypeChild = Type.Scalar

		assert(parentEnvironment.lookup(testTypeSymbol).isEmpty)

		parentEnvironment = parentEnvironment.putNew(testTypeSymbol)(testTypeParent)

		var childEnvironment = parentEnvironment.pushSubScope()
		childEnvironment = childEnvironment.putNew(testTypeSymbol)(testTypeChild)

		assert(childEnvironment.lookup(testTypeSymbol).nonEmpty)
		assert(childEnvironment.lookup(testTypeSymbol).get === testTypeChild)
		assert(parentEnvironment.lookup(testTypeSymbol).get === testTypeParent)
		assert(childEnvironment.isInScope(testTypeSymbol))

		val parentEnvironmentAgain = childEnvironment.popSubScope()
		assert(parentEnvironmentAgain.get.lookup(testTypeSymbol).get === testTypeParent)
	}
}
