package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Statement._
import com.preprocessor.ast.Ast.Term.Variable
import com.preprocessor.ast.Ast.Type.TypeAlias
import com.preprocessor.ast.Ast.{Type, Value, ValueSymbolDeclaration}
import com.preprocessor.ast.Symbol.{PropertySymbol, TypeSymbol, ValueSymbol}
import com.preprocessor.ast.{PropertyRecord, ValueRecord}
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success}

class StatementInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Statement interpreter"

	it should "correctly declare variables" in {
		val symbol = ValueSymbol("myVar")
		val varValue = Value.Scalar(123)

		val newState = run(VariableDeclaration(ValueSymbolDeclaration(symbol, None, varValue)))
		assert(newState.environment.isInCurrentScope(symbol))
		assert(newState.environment.lookup(symbol).get.value == varValue)
	}

	it should "reject declaration of existing variables" in {
		val symbol = ValueSymbol("myVar")
		val varType = Type.Number
		val varValue = Value.Scalar(123)
		val newState = state.withNewSymbol(symbol)(ValueRecord(varValue, varType)).get

		assertThrows[ProgramError](run(VariableDeclaration(ValueSymbolDeclaration(symbol, None, varValue)))(newState))
	}

	it should "reject declarations with wrong type annotations" in {
		val symbol = ValueSymbol("myVar")
		val varValue = Value.Scalar(123)
		val varType = Type.Boolean // Deliberately wrong type … that's the point here

		assertThrows[ProgramError](run(VariableDeclaration(ValueSymbolDeclaration(symbol, Some(varType), varValue))))
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
		).valueRecord.value == consequent)
	}

	it should "correctly create a type alias" in {
		val symbol = TypeSymbol("MyType")
		val newType = Type.Number
		val newState = run(TypeAliasDeclaration(TypeAlias(symbol), newType))

		assert(newState.environment.lookup(symbol).get == newType)
	}

	/*it should "allow narrowing type alias declarations" in {
		val symbol = TypeSymbol("MyType")
		val oldType = Type.Numeric
		val stateWithOldType = state.withUpdatedSymbol(symbol)(oldType).get
		val newType = Type.Number
		val newState = run(TypeAliasDeclaration(TypeAlias(symbol), newType))(stateWithOldType)

		assert(newState.environment.lookup(symbol).get == newType)
	}*/

	it should "reject non-narrowing type alias declarations" in {
		val symbol = TypeSymbol("MyType")
		val oldType = Type.Boolean
		val newState = state.withNewSymbol(symbol)(oldType).get
		val newType = Type.Number

		assertThrows[ProgramError](run(TypeAliasDeclaration(TypeAlias(symbol), newType))(newState))
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
		assert(newState.environment.lookupCurrent(PropertySymbol).get == List(
			PropertyRecord("position", "absolute"),
			PropertyRecord("width", "80%"),
			PropertyRecord("line-height", "1.6")
		))
	}

	it should "reject properties illegal in terms of types" in {
		assertThrows[ProgramError](run(Property(Value.Scalar(123), Value.Scalar(1.6))))
		assertThrows[ProgramError](run(
			Property(Value.String("width"), Value.Tuple2(Value.Percentage(80), Value.Percentage(80))))
		)
	}


	protected def run(expression: Statement)(implicit state: EvalState): EvalState = {
		val result = StatementInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
