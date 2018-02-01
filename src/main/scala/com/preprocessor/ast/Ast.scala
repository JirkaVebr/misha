package com.preprocessor.ast

import scala.collection.immutable.{Map => SMap}


/**
	* this object is deliberately rather lengthy ‒ we want to take advantage of the `sealed` keyword.
	*/
object Ast {

	sealed abstract class Node

	// Temporary
	case class Program(program: Expression.Expression) extends Node

	object Value {
		sealed abstract class Value extends Node

		sealed trait Primitive extends Value
		sealed trait Composite extends Value

		case class Number(value: Double, unit: UnitOfMeasure = UnitOfMeasure()) extends Primitive
		case class Boolean(value: scala.Boolean) extends Primitive
		case class String(value: java.lang.String) extends Primitive

		sealed trait Color extends Primitive
		case class Rgba(r: Short, g: Short, b: Short, a: Short) extends Color
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

		sealed trait Primitive extends Any
		sealed trait Composite extends Any

		case object Unit extends Primitive
		case object Boolean extends Primitive
		case object String extends Primitive
		case class Literal(value: Value.Primitive) extends Primitive

		case class Union(subs: Set[Any]) extends Composite
		case class Function(arguments: Seq[Any], output: Any) extends Composite
		case class Tuple2(first: Any, second: Any) extends Composite

		case class List(elements: Any) extends Composite
		case class Map(key: Any, value: Any, mandatoryEntries: Option[SMap[Any, Any]]) extends Composite


		sealed trait Numeric extends Primitive
		sealed trait Formula extends Numeric
		sealed trait Rational extends Formula
		sealed trait Dimensioned extends Formula
		sealed trait Number extends Rational

		case object Color extends Numeric

		case object Percentage extends Rational
		case object Ratio extends Rational

		case object Integer extends Number

		case object Length extends Dimensioned
		case object Angle extends Dimensioned
		case object Time extends Dimensioned
		case object Resolution extends Dimensioned
		case object Frequency extends Dimensioned
	}


	object Expression {
		sealed trait Expression extends Node

		case class SubExpression(value: Expression) extends Expression

		case class BinaryOperation(operator: BinaryOperator, left: Expression, right: Expression) extends Expression
		sealed trait BinaryOperator
		case object Addition extends BinaryOperator
		case object Subtraction extends BinaryOperator
		case object Multiplication extends BinaryOperator
		case object Division extends BinaryOperator
		case object Exponentiation extends BinaryOperator

		case class UnaryOperation(operator: UnaryOperator, value: Expression) extends Expression
		sealed trait UnaryOperator
		case object LogicNegation extends UnaryOperator
		case object ArithmeticNegation extends UnaryOperator

		case class Conditional(condition: Expression, consequent: Expression, alternative: Option[Expression])
			extends Expression

		//case class FunctionDeclaration
	}

	object Term {
		import Expression._

		sealed trait Term extends Expression

		case class Number(value: Double) extends Term
		case class Boolean(value: scala.Boolean) extends Term
		case class Color(r: Int, g: Int, b: Int, a: Int) extends Term
		case class String(value: java.lang.String) extends Term
		case class Variable(name: java.lang.String) extends Term
		case class FunctionCall(function: Expression, arguments: Seq[Expression]) extends Term
		case class List(items: Seq[Expression]) extends Term
		case class MemberAccess(`object`: Expression, name: Expression) extends Term
	}

}
