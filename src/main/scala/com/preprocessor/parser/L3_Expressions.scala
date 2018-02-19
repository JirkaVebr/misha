package com.preprocessor.parser

import com.preprocessor.ast.Ast
import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Statement._
import com.preprocessor.ast.Ast.Term._
import com.preprocessor.ast.Ast._
import org.parboiled2._

trait Expr
case class Num(n: Int) extends Expr
case class BinLop(l: Expr, r: Expr) extends Expr
case class BinRop1(l: Expr, r: Expr) extends Expr
case class BinRop2(l: Expr, r: Expr) extends Expr

trait L3_Expressions { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Literals
	with L2_Types =>


	def Expression: Rule1[Expression] = rule {
		assignment
	}

	private def assignment: Rule1[Expression] = rule { // Right associative
		logicalOr ~ zeroOrMore(
			"="  ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, r)) |
			"+=" ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Addition, l, r))) |
			"-=" ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Subtraction, l, r))) |
			"*=" ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Multiplication, l, r))) |
			"/=" ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Division, l, r))) |
			"^=" ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Exponentiation, l, r))) |
			"%=" ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Remainder, l, r)))
		)
	}

	private def logicalOr: Rule1[Expression] = rule { // Left associative
		logicalAnd ~ zeroOrMore(
			"||" ~!~ logicalAnd ~> ((l: Expression, r: Expression) => BinaryOperation(LogicalOr, l, r))
		)
	}

	private def logicalAnd: Rule1[Expression] = rule { // Left associative
		equalityComparison ~ zeroOrMore(
			"&&" ~!~ equalityComparison ~> ((l: Expression, r: Expression) => BinaryOperation(LogicalAnd, l, r))
		)
	}

	private def equalityComparison: Rule1[Expression] = rule { // Left associative
		otherComparison ~ zeroOrMore(
			("==" ~!~ otherComparison ~> ((l: Expression, r: Expression) => BinaryOperation(IsEqualTo, l, r))) |
			("!=" ~!~ otherComparison ~> ((l: Expression, r: Expression) =>
				UnaryOperation(LogicalNegation, BinaryOperation(IsEqualTo, l, r))))
		)
	}

	private def otherComparison: Rule1[Expression] = rule { // Left associative
		addition ~ zeroOrMore(
			("<=" ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(LowerEquals, l, r))) |
			("<" ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(LowerThan, l, r))) |
			(">=" ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(GreaterEquals, l, r))) |
			(">" ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(GreaterThan, l, r))) |
			("@in" ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(In, l, r)))
		)
	}

	private def addition: Rule1[Expression] = rule { // Left associative
		multiplication ~ zeroOrMore(
			("+" ~!~ multiplication ~> ((l: Expression, r: Expression) => BinaryOperation(Addition, l, r))) |
			("-" ~!~ multiplication ~> ((l: Expression, r: Expression) => BinaryOperation(Subtraction, l, r)))
		)
	}

	private def multiplication: Rule1[Expression] = rule { // Left associative
		exponentiation ~ zeroOrMore(
			("*" ~!~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Multiplication, l, r))) |
			("/" ~!~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Division, l, r))) |
			("%" ~!~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Remainder, l, r)))
		)
	}

	private def exponentiation: Rule1[Expression] = rule { // Right associative
		computedMemberAccess ~ zeroOrMore(
			"^" ~!~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Exponentiation, l, r))
		)
	}

	private def computedMemberAccess: Rule1[Expression] = rule {
		factor ~ zeroOrMore("[" ~ Expression ~ "]" ~> MemberAccess)
	}

	private def factor: Rule1[Expression] = rule {
		functionCall | conditional | delimitedList | unaryOperation | Variable | subExpression | magicSymbol | Literal |
		block
	}

	private def unaryOperation: Rule1[UnaryOperation] = rule {
		("-" ~ Expression ~> ((e: Expression) => UnaryOperation(ArithmeticNegation, e))) |
		("!" ~ Expression ~> ((e: Expression) => UnaryOperation(LogicalNegation, e)))
	}

	def Variable: Rule1[Term.Variable] = rule {
		// Deliberately using $ as a char and not a string as not to allow whitespace there
		'$' ~!~ variableName ~> ((name: String) => Term.Variable(name))
	}

	private def delimitedList: Rule1[Term.List] = rule {
		// They need to be separate rules as both body-subrules must allow empty input, and so if we had
		// "[" ~ (undelimitedListBody | delimitedListBody) ~ "]", things would always only work for one or the other
		// depending on the order.
		"[" ~!~
			(undelimitedListBody ~ "]" |
			delimitedListBody ~ "]") ~> Term.List
	}

	private def undelimitedList: Rule1[Term.List] = rule {
		undelimitedListBody ~> Term.List
	}

	private def undelimitedListBody: Rule1[Seq[Expression]] = rule {
		// We really need at least two items for it to be considered a list. Should one want to create a list of one item,
		// they should surround it with square brackets.
		(2 to Integer.MAX_VALUE).times(Expression).separatedBy(AnyWhitespace) ~ AnyWhitespace
	}

	private def delimitedListBody: Rule1[Seq[Expression]] = rule {
		zeroOrMore(Expression).separatedBy(",") ~ optional(",") ~ AnyWhitespace
	}

	private def variableName: Rule1[java.lang.String] = rule {
		Identifier
	}

	private def functionCall: Rule1[FunctionCall] = rule {
		// The error is just IntelliJ being dumb.
		(functionName ~ "(" ~ zeroOrMore(undelimitedList | Expression).separatedBy(",") ~ optional(",") ~ ")") ~> (
			(function: Expression, arguments: Seq[Expression]) => FunctionCall(function, arguments)
		)
	}

	private def functionName: Rule1[Expression] = rule {
		subExpression | Variable | (variableName ~> (
			(name: String) => Term.Variable(name)
		))
	}

	private def subExpression: Rule1[Expression] = rule {
		"(" ~ Expression ~ ")"
	}

	private def conditional: Rule1[Conditional] = rule {
		// The error is just IntelliJ being dumb.
		("@if" ~!~ subExpression ~ Expression ~ optional("@else" ~!~ Expression)) ~> (
			(condition: Expression, consequent: Expression, alternative: Option[Expression]) => Conditional(
				condition, consequent, alternative
			)
		)
	}

	// This isn't a literal, because it would make literal types involving it too tricky and difficult to use
	private def magicSymbol: Rule1[MagicSymbol] = rule {
		'&' ~ push(ParentSelector)
	}

	private def block: Rule1[Expression] = rule { // TODO replace '{' and '}' by INDENT and DEDENT respectively
		'{' ~!~ Statement ~ '}' ~!~ EndOfLine ~> Block
	}


	def Statement: Rule1[Statement] = rule {
		sequence
	}

	private def sequence: Rule1[Statement] = rule {
		sequenceNode ~ zeroOrMore(sequenceNode ~> Sequence)
	}

	private def sequenceNode: Rule1[Statement] = rule {
		(typeAliasDeclaration | rule | variableDeclaration | Expression) ~ EndOfLine
	}

	private def rule: Rule1[Ast.Statement.Rule] = rule { // TODO using quoted strings is temporary
		(QuotedString ~ EndOfLine ~ block) ~> Ast.Statement.Rule
	}

	private def typeAliasDeclaration: Rule1[TypeAliasDeclaration] = rule {
		(Token("@type") ~ MandatorySingleLineWhitespace ~ TypeAlias ~ SingleLineString("=") ~ Type) ~> TypeAliasDeclaration
	}

	private def variableDeclaration: Rule1[VariableDeclaration] = rule {
		(Token("@let") ~ MandatorySingleLineWhitespace ~ Variable ~ optional(":" ~!~ Type) ~ "=" ~!~ Expression) ~> (
			(variable: Variable, typeAnnotation: Option[Ast.Type.Any], value: Expression) =>
				VariableDeclaration(variable.name, typeAnnotation, value)
			)
	}

}
