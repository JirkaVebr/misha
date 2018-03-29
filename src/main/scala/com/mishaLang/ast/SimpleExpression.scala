package com.mishaLang.ast

import com.mishaLang.ast.Language.Expression.SimpleNumericOperator
import com.mishaLang.ast.Language.Value

object SimpleExpression {

	/**
		* These expressions are to be used within calc()
		*/
	sealed trait SimpleExpression

	case class BinaryOperation(operator: SimpleNumericOperator, left: SimpleExpression, right: SimpleExpression)
		extends SimpleExpression
	case class Term(value: Value.Number) extends SimpleExpression

}
