package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Statement._
import com.mishaLang.ast.Language.Term.Variable
import com.mishaLang.ast.Language.Type.TypeAlias
import com.mishaLang.ast.Language.{Type, Value, ValueSymbolDeclaration}
import com.mishaLang.ast.PropertyRecord
import com.mishaLang.error.ProgramError
import com.mishaLang.interpreter.Symbol.{PropertySymbol, TypeSymbol, ValueSymbol}

class StatementInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Statement interpreter"

	it should "correctly declare variables" in {
		val symbol = ValueSymbol("myVar")
		val varValue = Value.Scalar(123)

		val newState = run(VariableDeclaration(ValueSymbolDeclaration(symbol, None, varValue)))
		assert(newState.environment.isInCurrentScope(symbol))
		assert(newState.environment.lookup(symbol).get === varValue)
	}

	it should "reject declaration of existing variables" in {
		val symbol = ValueSymbol("myVar")
		val varValue = Value.Scalar(123)
		val newState = state.withNewSymbol(symbol)(varValue).get

		assertThrows[ProgramError[_]](run(VariableDeclaration(ValueSymbolDeclaration(symbol, None, varValue)))(newState))
	}

	it should "reject declarations with wrong type annotations" in {
		val symbol = ValueSymbol("myVar")
		val varValue = Value.Scalar(123)
		val varType = Type.Boolean // Deliberately wrong type … that's the point here

		assertThrows[ProgramError[_]](run(VariableDeclaration(ValueSymbolDeclaration(symbol, Some(varType), varValue))))
	}

	it should "correctly run a sequence" in {
		val symbol1 = ValueSymbol("myVar1")
		val symbol2 = ValueSymbol("myVar2")
		val varValue1 = Value.Scalar(123)
		val consequent = Value.Scalar(111)
		val alternative = Value.Scalar(222)

		assert(run(
			Sequence(
				Sequence(
					VariableDeclaration(ValueSymbolDeclaration(symbol1, None, varValue1)),
					VariableDeclaration(ValueSymbolDeclaration(symbol2, None,
						Conditional(BinaryOperation(IsEqualTo, Variable(symbol1), varValue1), consequent, Some(alternative))
					))
				),
				Variable(symbol2)
			)
		).value === consequent)
	}

	it should "correctly create a type alias" in {
		val symbol = TypeSymbol("MyType")
		val newType = Type.Scalar
		val newState = run(TypeAliasDeclaration(TypeAlias(symbol), newType))

		assert(newState.environment.lookup(symbol).get === newType)
	}

	/*it should "allow narrowing type alias declarations" in {
		val symbol = TypeSymbol("MyType")
		val oldType = Type.Numeric
		val stateWithOldType = state.withUpdatedSymbol(symbol)(oldType).get
		val newType = Type.Number
		val newState = run(TypeAliasDeclaration(TypeAlias(symbol), newType))(stateWithOldType)

		assert(newState.environment.lookup(symbol).get === newType)
	}*/

	it should "reject non-narrowing type alias declarations" in {
		val symbol = TypeSymbol("MyType")
		val oldType = Type.Boolean
		val newState = state.withNewSymbol(symbol)(oldType).get
		val newType = Type.Scalar

		assertThrows[ProgramError[_]](run(TypeAliasDeclaration(TypeAlias(symbol), newType))(newState))
	}

	it should "correctly add properties" in {
		val newState = run(
			Sequence(
				Sequence(
					Property(Value.String("line-height"), Value.Scalar(1.6)),
					Property(Value.String("width"), Value.Percentage(80))
				),
				Property(Value.String("position"), Value.String("absolute"))
			)
		)
		assert(newState.environment.lookupCurrent(PropertySymbol).get === List(
			PropertyRecord("position", "absolute"),
			PropertyRecord("width", "80%"),
			PropertyRecord("line-height", "1.6")
		))
	}

	it should "reject properties illegal in terms of types" in {
		assertThrows[ProgramError[_]](run(Property(Value.Scalar(123), Value.Scalar(1.6))))
		assertThrows[ProgramError[_]](run(
			Property(Value.String("width"), Value.Tuple2(Value.Percentage(80), Value.Percentage(80))))
		)
	}

	it should "correctly run a no-op" in {
		assert(run(NoOp).environment === testEnvironment)
	}


	protected def run(statement: Statement)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Statement](StatementInterpreter.run(_)(state), statement)
}
