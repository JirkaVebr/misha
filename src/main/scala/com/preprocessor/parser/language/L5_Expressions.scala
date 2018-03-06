package com.preprocessor.parser.language

import com.preprocessor.ast.Ast
import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Statement._
import com.preprocessor.ast.Ast.Term._
import com.preprocessor.ast.Ast._
import com.preprocessor.ast.Symbol.ValueSymbol
import com.preprocessor.parser._
import com.preprocessor.parser.common.{L0_Whitespace, L1_Strings, L2_Numbers}
import org.parboiled2._


trait L5_Expressions { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Literals
	with L4_Types =>

	import Characters._

	private val anyWhitespaceSeparator: () => Rule0 = () => rule (MandatoryAnyWhitespace)

	private val whitespaceSeparator: () => Rule0 = () => rule (MandatorySingleLineWhitespace)

	private val commaSeparator: () => Rule0 = () => rule (WhitespaceAround(","))

	private val expressionBody: () => Rule1[Expression] = () => rule (Expression)


	def Expression: Rule1[Expression] = rule {
		assignment
	}

	private def assignment: Rule1[Expression] = rule { // Right associative
		logicalOr ~ zeroOrMore(
			WhitespaceAround("=")  ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, r)) |
			WhitespaceAround("+=") ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Addition, l, r))) |
			WhitespaceAround("-=") ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Subtraction, l, r))) |
			WhitespaceAround("*=") ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Multiplication, l, r))) |
			WhitespaceAround("/=") ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Division, l, r))) |
			WhitespaceAround("^=") ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Exponentiation, l, r))) |
			WhitespaceAround("%=") ~!~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Remainder, l, r)))
		)
	}

	private def logicalOr: Rule1[Expression] = rule { // Left associative
		logicalAnd ~ zeroOrMore(
			WhitespaceAround("||") ~!~ logicalAnd ~> ((l: Expression, r: Expression) => BinaryOperation(LogicalOr, l, r))
		)
	}

	private def logicalAnd: Rule1[Expression] = rule { // Left associative
		equalityComparison ~ zeroOrMore(
			WhitespaceAround("&&") ~!~ equalityComparison ~> ((l: Expression, r: Expression) => BinaryOperation(LogicalAnd, l, r))
		)
	}

	private def equalityComparison: Rule1[Expression] = rule { // Left associative
		otherComparison ~ zeroOrMore(
			(WhitespaceAround("==") ~!~ otherComparison ~> ((l: Expression, r: Expression) => BinaryOperation(IsEqualTo, l, r))) |
			(WhitespaceAround("!=") ~!~ otherComparison ~> ((l: Expression, r: Expression) =>
				UnaryOperation(LogicalNegation, BinaryOperation(IsEqualTo, l, r))))
		)
	}

	private def otherComparison: Rule1[Expression] = rule { // Left associative
		addition ~ zeroOrMore(
			(WhitespaceAround("<=")  ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(LowerEquals, l, r))) |
			(WhitespaceAround("<")   ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(LowerThan, l, r))) |
			(WhitespaceAround(">=")  ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(GreaterEquals, l, r))) |
			(WhitespaceAround(">")   ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(GreaterThan, l, r))) |
			(WhitespaceAround("@in") ~!~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(In, l, r)))
		)
	}

	private def addition: Rule1[Expression] = rule { // Left associative
		multiplication ~ zeroOrMore(
			(WhitespaceAround("+") ~!~ multiplication ~> ((l: Expression, r: Expression) => BinaryOperation(Addition, l, r))) |
			(WhitespaceAround("-") ~!~ multiplication ~> ((l: Expression, r: Expression) => BinaryOperation(Subtraction, l, r)))
		)
	}

	private def multiplication: Rule1[Expression] = rule { // Left associative
		exponentiation ~ zeroOrMore(
			(WhitespaceAround("*") ~!~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Multiplication, l, r))) |
			(WhitespaceAround("/") ~!~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Division, l, r))) |
			(WhitespaceAround("%") ~!~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Remainder, l, r)))
		)
	}

	private def exponentiation: Rule1[Expression] = rule { // Right associative
		computedMemberAccess ~ zeroOrMore(
			WhitespaceAround("^") ~!~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Exponentiation, l, r))
		)
	}

	private def computedMemberAccess: Rule1[Expression] = rule {
		memberAccess ~ zeroOrMore("[" ~ Expression ~ "]" ~> MemberAccess)
	}

	private def memberAccess: Rule1[Expression] = rule {
		factor ~ zeroOrMore(NotAfterWhitespace ~ '.' ~ Identifier ~ AnyWhitespace ~> (
			(expression: Expression, identifier: String) => MemberAccess(expression, Value.String(identifier))
		))
	}

	private def factor: Rule1[Expression] = rule {
		functionCall | conditional | delimitedList | unaryOperation | Variable | anonymousFunction |
		subExpression | magicSymbol | Literal | block
	}

	private def unaryOperation: Rule1[UnaryOperation] = rule {
		("-" ~ Expression ~> ((e: Expression) => UnaryOperation(ArithmeticNegation, e))) |
		(!Flag ~ "!" ~ Expression ~> ((e: Expression) => UnaryOperation(LogicalNegation, e)))
	}

	def Variable: Rule1[Term.Variable] = rule {
		// Deliberately using $ as a char and not a string as not to allow whitespace there
		'$' ~!~ variableName ~> ((name: ValueSymbol) => Term.Variable(name))
	}

	private def delimitedList: Rule1[Term.List] = rule {
		// They need to be separate rules as both body-subrules must allow empty input, and so if we had
		// "[" ~ (undelimitedListBody | delimitedListBody) ~ "]", things would always only work for one or the other
		// depending on the order.
		'[' ~!~ AnyWhitespace ~
			((undelimitedListBody(anyWhitespaceSeparator) ~ AnyWhitespace ~ ']') |
				(delimitedListBody ~ AnyWhitespace ~ ']')) ~> Term.List
	}

	private def undelimitedList: Rule1[Term.List] = rule {
		undelimitedListBody(whitespaceSeparator) ~> Term.List
	}

	private def undelimitedListBody(separator: () => Rule0, content: () => Rule1[Expression] = expressionBody): Rule1[Seq[Expression]] = rule {
		// We really need at least two items for it to be considered a list. Should one want to create a list of one item,
		// they should surround it with square brackets.
		(2 to Int.MaxValue).times(content()).separatedBy(separator())
	}

	private def delimitedListBody: Rule1[Seq[Expression]] = rule {
		zeroOrMore(Expression).separatedBy(AnyWhitespaceAround(",")) ~
			optional(AnyWhitespace ~ ',') ~ // Don't match whitespace after this as any such whitespace will be
			AnyWhitespace // Can match any whitespace as the list will be terminated by something
	}

	private def variableName: Rule1[ValueSymbol] = rule {
		Identifier ~ AnyWhitespace ~> ValueSymbol
	}

	private def functionCall: Rule1[FunctionCall] = rule {
		// The error is just IntelliJ being dumb.
		(functionName ~ "(" ~ zeroOrMore(undelimitedList | Expression).separatedBy(",") ~ optional(",") ~ ")") ~> (
			(function: Expression, arguments: Seq[Expression]) => FunctionCall(function, arguments)
		)
	}

	private def functionName: Rule1[Expression] = rule {
		subExpression | Variable | (variableName ~> (
			(name: ValueSymbol) => Term.Variable(name)
		))
	}

	private def subExpression: Rule1[Expression] = rule {
		AnyWhitespaceAround("(") ~ Expression ~ ')'
	}

	private def conditional: Rule1[Conditional] = rule {
		// The error is just IntelliJ being dumb.
		(Token("@if") ~!~ subExpression ~ SingleLineWhitespace ~ Expression ~
			optional(WhitespaceAround("@else") ~!~ Expression)) ~> (
			(condition: Expression, consequent: Expression, alternative: Option[Expression]) => Conditional(
				condition, consequent, alternative
			)
		)
	}

	// This isn't a literal, because it would make literal types involving it too tricky and difficult to use
	private def magicSymbol: Rule1[MagicSymbol] = rule {
		'&' ~ push(ParentSelector)
	}

	private def block: Rule1[Block] = rule {
		SingleLineWhitespace ~ '\n' ~ INDENT ~!~ Statement ~ DEDENT ~> Block
	}

	private def anonymousFunction: Rule1[Expression] = rule {
		'(' ~ zeroOrMore(
			('$' ~ variableName ~ TypeAnnotation ~ optional(AnyWhitespaceAround("=") ~!~ Expression)) ~> (
				(name: ValueSymbol, typeAnnotation: Option[Ast.Type.Any], value: Option[Expression]) =>
					ValueSymbolDeclaration(name, typeAnnotation, value)
			)
		).separatedBy(AnyWhitespaceAround(",")) ~ optional(AnyWhitespace ~ ',') ~
			AnyWhitespaceAround(")") ~ TypeAnnotation ~ AnyWhitespaceAround("=>") ~ Expression ~> Value.Function
	}


	def Statement: Rule1[Statement] = rule {
		sequence
	}

	private def sequence: Rule1[Statement] = rule {
		sequenceNode ~ zeroOrMore(sequenceNode ~> Sequence)
	}

	private def sequenceNode: Rule1[Statement] = rule {
		(typeAliasDeclaration | variableDeclaration | property | rule | propertyFunctionCall | Expression) ~ EndOfLine
	}

	private def rule: Rule1[Ast.Statement.Rule] = rule { // TODO using quoted strings is temporary
		(QuotedString ~ SingleLineWhitespace ~ block) ~> Ast.Statement.Rule
	}

	private def typeAliasDeclaration: Rule1[TypeAliasDeclaration] = rule {
		(Token("@type") ~ MandatorySingleLineWhitespace ~ TypeAlias ~ WhitespaceAround("=") ~ Type) ~> TypeAliasDeclaration
	}

	private def variableDeclaration: Rule1[VariableDeclaration] = rule {
		(Token("@let") ~ MandatorySingleLineWhitespace ~ Variable ~ TypeAnnotation ~ WhitespaceAround("=") ~!~ Expression) ~> (
			(variable: Variable, typeAnnotation: Option[Ast.Type.Any], value: Expression) =>
				VariableDeclaration(ValueSymbolDeclaration(variable.name, typeAnnotation, value))
			)
	}

	private def property: Rule1[Property] = rule {
		(Token("@property") ~ "(" ~ Expression ~ "," ~ Expression ~ optional("," ~ Expression) ~ optional(",") ~ ")") ~> Property
	}

	private def propertyFunctionCall: Rule1[FunctionCall] = rule {
		Identifier ~ MandatorySingleLineWhitespace ~ oneOrMore(propertyFunctionArgument).separatedBy(MandatorySingleLineWhitespace) ~> (
			(identifier: String, arguments: Seq[Expression]) =>
				FunctionCall(Term.Variable(identifier), arguments)
		)
	}

	private def propertyFunctionArgument: Rule1[Expression] = rule {
		undelimitedListBody(commaSeparator, propertyFunctionArgumentInner) ~> Term.List | Expression
	}

	private val propertyFunctionArgumentInner: () => Rule1[Expression] = () => rule {
		undelimitedListBody(whitespaceSeparator) ~> Term.List | Expression
	}

}