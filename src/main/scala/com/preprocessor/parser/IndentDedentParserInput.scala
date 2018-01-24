package com.preprocessor.parser


import org.parboiled2.ParserInput
import org.parboiled2.ParserInput.CharArrayBasedParserInput
import org.parboiled2._

import scala.annotation.tailrec


class IndentDedentParserInput(val string: String) extends ParserInput {

	//private val convertedBuilder: StringBuilder = new BufferConverter(string).convert()
	private val convertedBuilder: StringBuilder = new StringBuilder(string)


	private class BufferConverter(val input: String) {
		val builder: StringBuilder = new StringBuilder()
		var cursor: Int = 0


		def convert(): StringBuilder = {
			skipWhitespaceLines()

			builder
		}


		private def skipWhitespaceLines(): Int = {
			var lineStartCursor = cursor


			lineStartCursor
		}
	}


	def charAt(ix: Int): Char =
		convertedBuilder.charAt(ix)


	def length: Int =
		convertedBuilder.length


	def sliceString(start: Int, end: Int): String =
		convertedBuilder.substring(start, math.min(end, string.length))


	def sliceCharArray(start: Int, end: Int): Array[Char] = {
		val chars = new Array[Char](end - start)
		convertedBuilder.copyToArray(chars, start, end - start)
		chars
	}


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

