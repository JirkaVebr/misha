package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Term.{FunctionCall, ParentSelector, Term, Variable}
import com.mishaLang.ast.Language.Type.Scalar
import com.mishaLang.ast.Language.Value.{Native, Value}
import com.mishaLang.ast.Language.{Term, Value}
import com.mishaLang.ast.Selector.{Class, Id, SelectorList}
import com.mishaLang.error.ProgramError
import com.mishaLang.interpreter.RuleContext.RuleSelector
import com.mishaLang.interpreter.Symbol.ValueSymbol

import scala.util.Success

class TermInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Term interpreter"

	it should "correctly read existing variables" in {
		val symbol = ValueSymbol("myVar")
		val varValue = Value.Scalar(123)
		val variable = Variable(symbol)
		val newState = state.withNewSymbol(symbol)(varValue).get

		assert(run(variable)(newState).value === varValue)
	}

	it should "reject undefined variable reads" in {
		assertThrows[ProgramError[_]](run(Variable(ValueSymbol("absentVar"))))
	}

	it should "correctly interpret tuples" in {
		assert(run(Term.Tuple2(
			UnaryOperation(LogicalNegation, Value.Boolean(true)), Value.String("foo")
		)).value === Value.Tuple2(
			Value.Boolean(false), Value.String("foo")
		))
	}

	it should "correctly interpret lists" in {
		assert(run(Term.List(List(
			BinaryOperation(Addition, Value.String("123"), Value.String("456")),
			BinaryOperation(IsEqualTo, Value.Scalar(10), Value.Scalar(10))
		))).value === Value.List(List(
			Value.String("123456"),
			Value.Boolean(true)
		)))
	}

	it should "correctly interpret the parent selector magic symbol" in {
		assert(run(ParentSelector).value === Value.List(List.empty))

		val originalRuleHead = ".myClass"
		val newState = EnvironmentWithValue(testEnvironment.pushSubScope(RuleSelector(Class("myClass"))))
		assert(run(ParentSelector)(newState).value === Value.List(List(originalRuleHead)))
	}

	it should "correctly invoke native functions" in {
		val multiply = Native(Vector(Scalar, Scalar), (values: Vector[Value]) => {
			val (a, b) = (values(0).asInstanceOf[Value.Scalar], values(1).asInstanceOf[Value.Scalar])
			Success(Value.Scalar(a.value * b.value))
		})

		assert(run(FunctionCall(
			multiply, Vector(Value.Scalar(3), Value.Scalar(5))
		)).value === Value.Scalar(15))

		assertThrows[ProgramError[_]](run(FunctionCall(
			multiply, Vector(Value.Scalar(3))
		)))

		assertThrows[ProgramError[_]](run(FunctionCall(
			multiply, Vector(Value.Scalar(3), Value.Scalar(3), Value.Scalar(3))
		)))

		assertThrows[ProgramError[_]](run(FunctionCall(
			multiply, Vector(Value.Scalar(3), Value.String("bad type"))
		)))
	}

	protected def run(term: Term)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Term](TermInterpreter.run(_), term)
}
