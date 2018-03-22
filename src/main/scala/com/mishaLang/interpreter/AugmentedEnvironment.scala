package com.mishaLang.interpreter

import com.mishaLang.ast.Language
import com.mishaLang.error.ProgramError.ProgramErrorCode
import com.mishaLang.error.{Error, ProgramError}
import com.mishaLang.interpreter.Symbol.Symbol

import scala.util.{Failure, Success}


case class AugmentedEnvironment[T](environment: Environment, value: T) {

	def ~>(newValue: T): Success[AugmentedEnvironment[T]] =
		Success(AugmentedEnvironment[T](environment, newValue))

	def withUpdatedSymbol(symbol: Symbol)(symbolValue: symbol.Value): Success[AugmentedEnvironment[T]] =
		Success(AugmentedEnvironment[T](environment.updated(symbol)(symbolValue), value))

	def withNewSymbol(symbol: Symbol)(symbolValue: symbol.Value): Success[AugmentedEnvironment[T]] =
		Success(AugmentedEnvironment[T](environment.putNew(symbol)(symbolValue), value))

	def fail(errorCode: ProgramErrorCode, nodes: Language.Node*): Failure[AugmentedEnvironment[T]] =
		Failure(ProgramError[T](errorCode, this, nodes: _*))

	def failFatally(error: Error): Failure[AugmentedEnvironment[T]] = Failure(error)
}
