package com.preprocessor.ast

import com.preprocessor.ast.Ast.Expression.Expression
import com.preprocessor.ast.NumberUnit.UnitOfMeasure
import com.preprocessor.ast.Symbol.{TypeSymbol, ValueSymbol}
import com.preprocessor.interpreter.typing.{Subtype, Typing}

import scala.collection.immutable.{Map => SMap}


/**
	* this object is deliberately rather lengthy ‒ we want to take advantage of the `sealed` keyword.
	*/
object Ast {

	sealed abstract class Node

	case class Program(program: Statement.Statement) extends Node

	case class ValueSymbolDeclaration[D](name: ValueSymbol, typeAnnotation: Option[Type.Any], value: D)

	object Value {
		import Term._

		sealed abstract class Value() extends Term {
			def valueType: Type.Any = Typing.getType(this)
		}

		case object Unit extends Value

		sealed abstract class Primitive extends Value
		sealed abstract class Composite extends Value

		sealed abstract class Number(val value: Double) extends Primitive
		case class Dimensioned(override val value: Double, unit: UnitOfMeasure) extends Number(value)
		case class Scalar(override val value: Double) extends Number(value)
		case class Percentage(override val value: Double) extends Number(value)


		case class Boolean(value: scala.Boolean) extends Primitive
		case class String(value: java.lang.String) extends Primitive

		sealed abstract class Color extends Primitive
		case class Rgba(r: Int, g: Int, b: Int, a: Int = 255) extends Color
		case object CurrentColor extends Color
		case object Transparent extends Color

		sealed abstract class Flag extends Primitive
		case object Important extends Flag
		case object Duplicate extends Flag

		// Composite types
		case class Tuple2Value(first: Value, second: Value) extends Composite
		case class List(values: Seq[Value]) extends Composite
		case class Function(arguments: Seq[ValueSymbolDeclaration[Option[Expression]]], returnType: Option[Type.Any], body: Expression)
			extends Composite
	}


	object Type {
		sealed trait Any extends Node {
			def isSubtypeOf(otherType: Any): Boolean = Subtype.isSubtypeOf(this, otherType)
			def isEquivalentTo(otherType: Any): Boolean = Subtype.isEquivalentTo(this, otherType)
		}
		case object Any extends Any

		sealed trait Primitive extends Any
		sealed trait Composite extends Any

		case object Unit extends Primitive
		case object Boolean extends Primitive
		case object String extends Primitive
		case object Flag extends Primitive
		case class Literal(value: Value.Primitive) extends Primitive
		case class TypeAlias(name: TypeSymbol) extends Any

		case class Union(subs: Set[Any]) extends Composite
		case class Function(arguments: Seq[Any], output: Any) extends Composite
		case class Tuple2(first: Any, second: Any) extends Composite
		case class Subtraction(minuend: Any, subtrahend: Any) extends Composite

		case class List(of: Any) extends Composite
		case class Map(key: Any, value: Any, mandatoryEntries: Option[SMap[Any, Any]]) extends Composite


		case object Formula extends Primitive // e.g. 100% - 10px
		case object Numeric extends Primitive // e.g. #ddd or 123vh, that is values that define arithmetic operations
		case object Color extends Primitive

		case object Number extends Primitive // A concrete number, e.g. 123.456
		case object Scalar extends Primitive // A unitless number
		case class Dimensioned(typeName: TypeSymbol) extends Primitive // A number with a unit
		case object Percentage extends Primitive
	}

	object Statement {
		import Expression.Expression
		import Type.{Any, TypeAlias}

		sealed trait Statement extends Node

		case class Sequence(current: Statement, following: Statement) extends Statement
		case class Property(name: Expression, value: Expression, flags: Option[Expression] = None) extends Statement
		//case class Import(destination: Expression, parameters: Option[Expression]) extends Statement
		case class TypeAliasDeclaration(alias: TypeAlias, subType: Any) extends Statement
		case class VariableDeclaration(declaration: ValueSymbolDeclaration[Expression]) extends Statement

		//case class Rule(head: StringInterpolation, body: Statement) extends Statement
		case class Rule(head: Value.String, body: Statement) extends Statement // TODO
	}


	object Expression {
		import Statement._

		sealed trait Expression extends Statement

		case class BinaryOperation(operator: BinaryOperator, left: Expression, right: Expression) extends Expression
		sealed trait BinaryOperator

		sealed trait NumericOperator extends BinaryOperator
		case object Addition extends NumericOperator
		case object Subtraction extends NumericOperator
		case object Multiplication extends NumericOperator
		case object Division extends NumericOperator
		case object Exponentiation extends NumericOperator
		case object Remainder extends NumericOperator

		sealed trait Comparison extends BinaryOperator
		case object IsEqualTo extends Comparison
		case object In extends Comparison
		case object LowerThan extends Comparison
		case object LowerEquals extends Comparison
		case object GreaterThan extends Comparison
		case object GreaterEquals extends Comparison

		sealed trait LogicalOperator extends BinaryOperator
		case object LogicalAnd extends LogicalOperator
		case object LogicalOr extends LogicalOperator

		sealed trait Assignment extends BinaryOperator
		case object Equals extends Assignment

		case class UnaryOperation(operator: UnaryOperator, value: Expression) extends Expression
		sealed trait UnaryOperator
		case object LogicalNegation extends UnaryOperator
		case object ArithmeticNegation extends UnaryOperator

		case class StringInterpolation(components: Seq[Either[String, Expression]]) extends Expression
		case class Conditional(condition: Expression, consequent: Expression, alternative: Option[Expression])
			extends Expression
		case class Block(content: Statement) extends Expression
	}

	object Term {
		import Expression._

		sealed trait Term extends Expression

		sealed trait MagicSymbol extends Term
		case object ParentSelector extends MagicSymbol

		case class Variable(name: ValueSymbol) extends Term
		case class FunctionCall(function: Expression, arguments: Seq[Expression] = scala.Vector.empty) extends Term
		case class List(items: Seq[Expression]) extends Term
		case class MemberAccess(container: Expression, name: Expression) extends Term
	}

}
