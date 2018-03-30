package com.mishaLang

import java.io.PrintWriter


object Main extends App {

	val output = Compiler.compileFile("src/scratch/test.mi")

	new PrintWriter("src/scratch/out") {
		write(output)
		close()
	}

}
