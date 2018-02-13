package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Program

class Interpreter(val program: Program) {

	val rootEnvironment: RootEnvironment = createRootEnvironment()

	def runProgram(): EvalState =
		StatementInterpreter.run(program.program)(EvalState(rootEnvironment))

	private def createRootEnvironment(): RootEnvironment =
		new RootEnvironment // TODO, this may get more involved
}
