package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Term.{FunctionCall, ParentSelector, Term, Variable}
import com.mishaLang.ast.Language.Type.Scalar
import com.mishaLang.ast.Language.Value.{Lambda, Native, PolymorphicGroup, Value}
import com.mishaLang.ast.Language.{Term, Type, Value, ValueSymbolDeclaration}
import com.mishaLang.ast.Selector.Class
import com.mishaLang.error.ProgramError
import com.mishaLang.interpreter.RuleContext.RuleSelector
import com.mishaLang.interpreter.Symbol.ValueSymbol

import scala.util.Success

class TermInterpreterSpec extends BaseInterpreterSpec {

	private val scopeId = testEnvironment.scopeId

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
		))).value === Value.List(Vector(
			Value.String("123456"),
			Value.Boolean(true)
		)))
	}

	it should "correctly interpret the parent selector magic symbol" in {
		assert(run(ParentSelector).value === Value.List(Vector()))

		val originalRuleHead = ".myClass"
		val newState = EnvironmentWithValue(testEnvironment.pushSubScope(RuleSelector(Class("myClass"))).get)
		assert(run(ParentSelector)(newState).value === Value.List(Vector(originalRuleHead)))
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

	it should "correctly invoke nullary lambdas" in {
		val lambda = Lambda(None, Vector(), Vector(), None, Block(
			Value.Scalar(123)
		), scopeId)
		assert(run(FunctionCall(lambda, Vector())).value === Value.Scalar(123))
	}


	private val concatenateStrings = Lambda(None, Vector(
		ValueSymbolDeclaration[Unit]("str1", Some(Type.String), Unit),
		ValueSymbolDeclaration[Unit]("str2", Some(Type.String), Unit)
	), Vector(), None, Block(
		BinaryOperation(Addition, Variable("str1"), Variable("str2"))
	), scopeId)


	it should "reject lambda function calls with incorrect arity" in {
		assertThrows[ProgramError[_]](run(FunctionCall(
			concatenateStrings, Vector(Value.String("foo"))
		)))

		assertThrows[ProgramError[_]](run(FunctionCall(
			concatenateStrings, Vector(Value.String("foo"), Value.String("bar"), Value.String("baz"))
		)))
	}

	it should "reject lambda function calls with ill-typed arguments" in {
		assertThrows[ProgramError[_]](run(FunctionCall(
			concatenateStrings, Vector(Value.Scalar(1), Value.String("foo"))
		)))
	}

	it should "correctly invoke lambdas with just mandatory arguments" in {
		assert(run(FunctionCall(concatenateStrings, Vector(
			Value.String("abc"), Value.String("def")
		))).value === Value.String("abcdef"))
	}

	it should "correctly invoke lambdas with just supplied optional arguments" in {
		val lambda = Lambda(None, Vector(
			ValueSymbolDeclaration[Unit]("str1", None, Unit),
			ValueSymbolDeclaration[Unit]("str2", None, Unit)
		), Vector(
			ValueSymbolDeclaration[Expression]("str3", None, Value.String("default"))
		), None, Block(
			BinaryOperation(Addition, BinaryOperation(Addition, Variable("str1"), Variable("str2")), Variable("str3"))
		), scopeId)
		assert(run(FunctionCall(lambda, Vector(
			Value.String("abc"), Value.String("def"), Value.String("ghi")
		))).value === Value.String("abcdefghi"))
	}

	it should "correctly invoke lambdas with just missing optional arguments" in {
		val lambda = Lambda(None, Vector(
			ValueSymbolDeclaration[Unit]("str1", None, Unit),
			ValueSymbolDeclaration[Unit]("str2", None, Unit)
		), Vector(
			ValueSymbolDeclaration[Expression]("str3", None, Value.String("default"))
		), None, Block(
			BinaryOperation(Addition, BinaryOperation(Addition, Variable("str1"), Variable("str2")), Variable("str3"))
		), scopeId)
		assert(run(FunctionCall(lambda, Vector(
			Value.String("abc"), Value.String("def")
		))).value === Value.String("abcdefdefault"))
	}

	it should "correctly invoke lambdas with some supplied and some missing optional arguments" in {
		val lambda = Lambda(None, Vector(
			ValueSymbolDeclaration[Unit]("str1", None, Unit)
		), Vector(
			ValueSymbolDeclaration[Expression]("str2", None, Value.String("default1")),
			ValueSymbolDeclaration[Expression]("str3", None, Value.String("default2"))
		), None, Block(
			BinaryOperation(Addition, BinaryOperation(Addition, Variable("str1"), Variable("str2")), Variable("str3"))
		), scopeId)
		assert(run(FunctionCall(lambda, Vector(
			Value.String("abc"), Value.String("def")
		))).value === Value.String("abcdefdefault2"))
	}

	it should "correctly invoke recursive lambdas" in {
		val lambda = Lambda(Some("factorial"), Vector(
			ValueSymbolDeclaration[Unit]("n", None, Unit)
		), Vector(), None, Block(
			Conditional(
				BinaryOperation(IsEqualTo, Variable("n"), Value.Scalar(0)),
				Value.Scalar(1),
				Some(BinaryOperation(Multiplication, Variable("n"), FunctionCall(
					Variable("factorial"), Vector(
						BinaryOperation(Subtraction, Variable("n"), Value.Scalar(1))
					)
				)))
			)
		), scopeId)
		assert(run(FunctionCall(lambda, Vector(Value.Scalar(5)))).value === Value.Scalar(120))
	}

	it should "pass parent context to lambdas" in {
		val testLambda = ValueSymbol("testLambda")
		val testSelector = Class("testClass")
		val root = testEnvironment.putNew(testLambda)(Value.Lambda(
			None, Vector(), Vector(), None, Term.ParentSelector, testEnvironment.scopeId
		))

		assert(
			run(Term.FunctionCall(Term.Variable(testLambda), Vector()))(EnvironmentWithValue(root)).value ===
			Value.List(Vector())
		)

		val sub0 = root.pushSubScope(RuleSelector(testSelector)).get

		assert(
			run(Term.FunctionCall(Term.Variable(testLambda), Vector()))(EnvironmentWithValue(sub0)).value ===
				Value.List(Vector(testSelector.toString))
		)
	}


	it should "correctly invoke polymorphic groups" in {
		val add = PolymorphicGroup(Vector( // This is silly. It's just for testing though.
			Lambda(None, Vector(
				ValueSymbolDeclaration[Unit]("num1", Some(Type.Scalar), Unit),
				ValueSymbolDeclaration[Unit]("num2", Some(Type.Scalar), Unit)
			), Vector(), None, Block(
				BinaryOperation(Addition, Variable("num1"), Variable("num2"))
			), scopeId),
			Lambda(None, Vector(
				ValueSymbolDeclaration[Unit]("str1", Some(Type.String), Unit),
				ValueSymbolDeclaration[Unit]("str2", Some(Type.String), Unit)
			), Vector(), None, Block(
				BinaryOperation(Addition, Variable("str1"), Variable("str2"))
			), scopeId)
		))

		assert(run(FunctionCall(add, Vector(
			Value.String("abc"), Value.String("def")
		))).value === Value.String("abcdef"))
		assert(run(FunctionCall(add, Vector(
			Value.Scalar(123), Value.Scalar(456)
		))).value === Value.Scalar(579))
	}

	protected def run(term: Term)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Term](TermInterpreter.run(_)(state), term)
}
