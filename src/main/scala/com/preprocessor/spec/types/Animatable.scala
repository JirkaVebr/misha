package com.preprocessor.spec.types
import com.preprocessor.ast.Language.{Type, Value}

object Animatable extends Type {

	override def name: String = "Animatable"

	override def apply(): Type.Any = Type.Union(Set(
			Type.Literal("all"),
			Type.Literal("backdrop-filter"),
			Type.Literal("background"),
			Type.Literal("background-color"),
			Type.Literal("background-position"),
			Type.Literal("background-size"),
			Type.Literal("border"),
			Type.Literal("border-bottom"),
			Type.Literal("border-bottom-color"),
			Type.Literal("border-bottom-left-radius"),
			Type.Literal("border-bottom-right-radius"),
			Type.Literal("border-bottom-width"),
			Type.Literal("border-color"),
			Type.Literal("border-left"),
			Type.Literal("border-left-color"),
			Type.Literal("border-left-width"),
			Type.Literal("border-radius"),
			Type.Literal("border-right"),
			Type.Literal("border-right-color"),
			Type.Literal("border-right-width"),
			Type.Literal("border-top"),
			Type.Literal("border-top-color"),
			Type.Literal("border-top-left-radius"),
			Type.Literal("border-top-right-radius"),
			Type.Literal("border-top-width"),
			Type.Literal("border-width"),
			Type.Literal("bottom"),
			Type.Literal("box-shadow"),
			Type.Literal("caret-color"),
			Type.Literal("clip"),
			Type.Literal("clip-path"),
			Type.Literal("color"),
			Type.Literal("column-count"),
			Type.Literal("column-gap"),
			Type.Literal("column-rule"),
			Type.Literal("column-rule-color"),
			Type.Literal("column-rule-width"),
			Type.Literal("column-width"),
			Type.Literal("columns"),
			Type.Literal("filter"),
			Type.Literal("flex"),
			Type.Literal("flex-basis"),
			Type.Literal("flex-grow"),
			Type.Literal("flex-shrink"),
			Type.Literal("font"),
			Type.Literal("font-size"),
			Type.Literal("font-size-adjust"),
			Type.Literal("font-stretch"),
			Type.Literal("font-variation-settings"),
			Type.Literal("font-weight"),
			Type.Literal("grid-column-gap"),
			Type.Literal("grid-gap"),
			Type.Literal("grid-row-gap"),
			Type.Literal("height"),
			Type.Literal("left"),
			Type.Literal("letter-spacing"),
			Type.Literal("line-height"),
			Type.Literal("margin"),
			Type.Literal("margin-bottom"),
			Type.Literal("margin-left"),
			Type.Literal("margin-right"),
			Type.Literal("margin-top"),
			Type.Literal("mask"),
			Type.Literal("mask-border"),
			Type.Literal("mask-position"),
			Type.Literal("mask-size"),
			Type.Literal("max-height"),
			Type.Literal("max-width"),
			Type.Literal("min-height"),
			Type.Literal("min-width"),
			Type.Literal("object-position"),
			Type.Literal("offset"),
			Type.Literal("offset-anchor"),
			Type.Literal("offset-distance"),
			Type.Literal("offset-path"),
			Type.Literal("offset-position"),
			Type.Literal("offset-rotate"),
			Type.Literal("opacity"),
			Type.Literal("order"),
			Type.Literal("outline"),
			Type.Literal("outline-color"),
			Type.Literal("outline-offset"),
			Type.Literal("outline-width"),
			Type.Literal("padding"),
			Type.Literal("padding-bottom"),
			Type.Literal("padding-left"),
			Type.Literal("padding-right"),
			Type.Literal("padding-top"),
			Type.Literal("perspective"),
			Type.Literal("perspective-origin"),
			Type.Literal("right"),
			Type.Literal("rotate"),
			Type.Literal("scale"),
			Type.Literal("scroll-snap-coordinate"),
			Type.Literal("scroll-snap-destination"),
			Type.Literal("shape-image-threshold"),
			Type.Literal("shape-margin"),
			Type.Literal("shape-outside"),
			Type.Literal("tab-size"),
			Type.Literal("text-decoration"),
			Type.Literal("text-decoration-color"),
			Type.Literal("text-emphasis"),
			Type.Literal("text-emphasis-color"),
			Type.Literal("text-indent"),
			Type.Literal("text-shadow"),
			Type.Literal("top"),
			Type.Literal("transform"),
			Type.Literal("transform-origin"),
			Type.Literal("translate"),
			Type.Literal("vertical-align"),
			Type.Literal("visibility"),
			Type.Literal("width"),
			Type.Literal("word-spacing"),
			Type.Literal("z-index"))
	))
}