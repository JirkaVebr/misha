package com.preprocessor.error

import org.parboiled2

class ParseError(val parseError: parboiled2.ParseError) extends Error {

}
