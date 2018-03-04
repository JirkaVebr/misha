package com.preprocessor.parser


import com.preprocessor.error.InputError
import org.parboiled2.ParserInput

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


/**
	* This class is very heavily inspired by the original IndentDedentInputBuffer from parboiled (not parboiled2) and
	* the way Python handles the same problem.
	* @see https://github.com/sirthias/parboiled/blob/ed7b6f2456e27e9eafefdfd2fd2fbe50fc4e47cd/parboiled-core/src/main/java/org/parboiled/buffers/IndentDedentInputBuffer.java
	* @see https://github.com/python/cpython/blob/master/Parser/tokenizer.c
	* @param originalInput string
	*/
class IndentDedentParserInput(val originalInput: String) extends ParserInput {

	private val converter = new InputConverter(originalInput)
	private val convertedInput: String = converter.getConvertedInput



	private class InputConverter(val input: String) {
		import Characters._

		private val inputBuilder: InputBuilder = new InputBuilder()

		private val tabWidth: Int = 4

		private var cursor: Int = 0
		private var currentChar: Char = input.charAt(0)
		private var indentLevels: List[Int] = List(0)
		private var insideSingleQuotedString: Boolean = false
		private var insideDoubleQuotedString: Boolean = false

		convert()

		def getConvertedInput: String =
			inputBuilder.getConvertedInput

		private def convert(): Unit = {
			//skipWhitespaceLines()

			var currentIndentLevel: Int = processIndent()

			if (currentIndentLevel != 0) {
				throw new InputError // Unexpected indent
			}

			//noinspection LoopVariableNotUpdated
			while (currentChar != EOI) {
				// Right here we're at the beginning of a new line

				val commentCharsStripped: Int = processComment()

				if (currentChar != '\n' && currentChar != EOI) {
					inputBuilder.append(currentChar)
					advance()
				} else if (commentCharsStripped != 0) {
					inputBuilder.registerComment(commentCharsStripped)
					advance()
				} else {
					inputBuilder.append('\n')
					advance()

					val newIndentLevel: Int = processIndent()

					if (newIndentLevel > currentIndentLevel) {
						indentLevels = newIndentLevel :: indentLevels
						currentIndentLevel = newIndentLevel
						inputBuilder.append(INDENT)
					} else {
						while (newIndentLevel < currentIndentLevel && newIndentLevel <= indentLevels.head) {
							indentLevels = indentLevels.tail
							currentIndentLevel = indentLevels.head
							inputBuilder.append(DEDENT)
						}
						if (newIndentLevel != currentIndentLevel) {
							throw new InputError
						}
					}
				}
			}

			if (indentLevels.length > 1) {
				inputBuilder.append('\n')

				while (indentLevels.length > 1) {
					indentLevels = indentLevels.tail
					inputBuilder.append(DEDENT)
				}
			}

			if (inputBuilder.currentChar != '\n') {
				inputBuilder.append('\n')
			}
		}

		private def advance(): Unit = {
			currentChar = nextChar()
			cursor += 1
		}

		private def nextChar(): Char = {
			try {
				input.charAt(cursor + 1)
			} catch {
				case _: StringIndexOutOfBoundsException => EOI
			}
		}

		private def processIndent(): Int = {
			@tailrec def processChar(indentLevel: Int): Int = {
				currentChar match {
					case ' ' =>
						advance()
						processChar(indentLevel + 1)
					case '\t' =>
						advance()
						processChar((indentLevel / tabWidth + 1) * tabWidth)
					case '\n' => // We found an empty line.
						advance()
						processChar(0)
					case EOI =>
						0
					case _ =>
						indentLevel
				}
			}

			processChar(0) // We start at the beginning of a line
		}

		private def processComment(): Int = {
			if (insideSingleQuotedString || insideDoubleQuotedString)
				0
			else if (currentChar == '/' && nextChar() == '/') {
				val pastCursor: Int = cursor

				while (currentChar != '\n' && currentChar != EOI)
					advance()

				cursor - pastCursor
			} else {
				0
			}
		}


		private class InputBuilder {
			private val builder: StringBuilder = StringBuilder.newBuilder
			private val keyMapBuffer: ArrayBuffer[Int] = new ArrayBuffer[Int]

			def append(char: Char): Unit = {
				keyMapBuffer.append(cursor)
				builder.append(char)
			}

			def registerComment(commentCharCount: Int): Unit = {
				keyMapBuffer.append(cursor - commentCharCount)
			}

			def getConvertedInput: String =
				builder.mkString

			def getKeyMap: Vector[Int] =
				keyMapBuffer.toVector

			def currentChar: Char =
				try {
					builder.last
				} catch {
					case _: NoSuchElementException => EOI
				}
		}
	}


	def charAt(ix: Int): Char = // Hot
		convertedInput.charAt(ix)


	def length: Int = // Hot
		convertedInput.length


	def sliceString(start: Int, end: Int): String =
		convertedInput.substring(start, math.min(end, convertedInput.length))


	def sliceCharArray(start: Int, end: Int): Array[Char] = {
		val chars = new Array[Char](end - start)
		convertedInput.copyToArray(chars, start, end - start)
		chars
	}


	// TODO: This yields completely silly results at the moment as it works on the original input.
	// Need to figure this out when implementing error reporting.
	def getLine(line: Int): String = {
		@tailrec def rec(ix: Int, lineStartIx: Int, lineNr: Int): String =
			if (ix < length)
				if (charAt(ix) == '\n')
					if (lineNr < line) rec(ix + 1, ix + 1, lineNr + 1)
					else sliceString(lineStartIx, ix)
				else rec(ix + 1, lineStartIx, lineNr)
			else if (lineNr == line) sliceString(lineStartIx, ix) else ""
		rec(ix = 0, lineStartIx = 0, lineNr = 1)
	}

}

