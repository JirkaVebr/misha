package com.preprocessor.ast

case class CssIdentifier(value: String) {

	override def toString: String = value
}
