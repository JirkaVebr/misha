package com.preprocessor.ast

import com.preprocessor.ast.UnitOfMeasure.{Scalar, UnitOfMeasure}

import scala.collection.immutable.{Map => SMap}


/**
	* this object is deliberately rather lengthy ‒ we want to take advantage of the `sealed` keyword.
	*/
object Ast {

	sealed abstract class Node

	// Temporary
	case class Program(program: Expression.Expression) extends Node

	object Value {
		import Term._

		sealed abstract class Value extends Node with Term

		sealed trait Primitive extends Value
		sealed trait Composite extends Value

		case class Number(value: Double, unit: UnitOfMeasure = Scalar()) extends Primitive
		case class Boolean(value: scala.Boolean) extends Primitive
		case class String(value: java.lang.String) extends Primitive

		sealed trait Color extends Primitive
		case class Rgba(r: Int, g: Int, b: Int, a: Int = 0) extends Color
		case object CurrentColor extends Color
		case object Transparent extends Color

		sealed trait Flag extends Primitive
		case object Important extends Flag

		// Composite types
		case class Tuple2Value(first: Value, second: Value) extends Composite
		case class List(values: Seq[Value]) extends Composite
		//case class FunctionValue(arguments: Option[Seq[]]) extends Composite
	}


	object Type {
		sealed trait Any extends Node
		case object Any extends Any

		sealed trait Primitive extends Any
		sealed trait Composite extends Any

		case object Unit extends Primitive
		case object Boolean extends Primitive
		case object String extends Primitive
		case class Literal(value: Value.Primitive) extends Primitive
		case class TypeAlias(name: String) extends Any

		case class Union(subs: Set[Any]) extends Composite
		case class Function(arguments: Seq[Any], output: Any) extends Composite
		case class Tuple2(first: Any, second: Any) extends Composite
		case class Subtraction(minuend: Any, subtrahend: Any) extends Composite

		case class List(elements: Any) extends Composite
		case class Map(key: Any, value: Any, mandatoryEntries: Option[SMap[Any, Any]]) extends Composite


		sealed trait Numeric extends Primitive
		sealed trait Formula extends Numeric
		sealed trait Rational extends Formula
		sealed trait Number extends Rational

		case object Color extends Numeric
		case object Dimensioned extends Formula
		case object Formula extends Formula
		case object Integer extends Number
		case object Number extends Number
		case object Numeric extends Numeric
		case object Percentage extends Rational
		case object Rational extends Rational
		case object Ratio extends Rational
	}

	object Statement {
		import Expression.Expression
		import Type.{Any, TypeAlias}

		sealed trait Statement extends Node

		case object NoOp extends Statement
		case class Sequence(current: Statement, following: Statement) extends Statement
		//case class Property(name: Expression, value: Expression, flags: Option[Expression]) extends Statement
		//case class Import(destination: Expression, parameters: Option[Expression]) extends Statement
		case class TypeAliasDeclaration(alias: TypeAlias, subType: Any) extends Statement
		case class VariableDeclaration(name: String, typeAnnotation: Option[Any], value: Expression) extends Statement
		case class FunctionDeclaration(name: String, typeAnnotation: Option[Any], value: Statement) extends Statement

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

		case class Conditional(condition: Expression, consequent: Expression, alternative: Option[Expression])
			extends Expression
		case class StringInterpolation(components: Seq[Either[String, Expression]]) extends Expression
	}

	object Term {
		import Expression._

		sealed trait Term extends Expression

		case class Variable(name: java.lang.String) extends Term
		case class FunctionCall(function: Expression, arguments: Seq[Expression] = scala.Vector.empty) extends Term
		case class List(items: Seq[Expression]) extends Term
		case class MemberAccess(`object`: Expression, name: Expression) extends Term
	}

}
