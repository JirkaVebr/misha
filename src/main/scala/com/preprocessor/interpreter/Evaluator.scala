package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Value._
import com.preprocessor.ast.Ast._


class Evaluator(val program: Program) {

	def evaluate(): Value = evaluate(program.program)

	protected def evaluate(expression: Expression): Value = ExpressionEvaluator.evaluate(expression)
}
