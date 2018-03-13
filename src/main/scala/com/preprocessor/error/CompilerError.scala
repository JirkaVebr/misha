package com.preprocessor.error

case class CompilerError(message: String, usefulData: Any*) extends Error
