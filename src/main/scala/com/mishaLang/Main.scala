package com.mishaLang


object Main extends App {

	val timeStart = System.nanoTime()
	val output = Compiler.compileFile("src/scratch/test.mi")
	val timeEnd = System.nanoTime()
	println(s"Time: ${(timeEnd - timeStart) / 1e6}ms")
	println(output)

}
