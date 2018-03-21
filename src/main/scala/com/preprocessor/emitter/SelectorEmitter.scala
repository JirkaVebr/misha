package com.preprocessor.emitter

import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace, NoNamespace}
import com.preprocessor.ast.Selector._
import com.preprocessor.spec.PseudoClasses
import com.preprocessor.spec.PseudoClasses.AnPlusB
import com.preprocessor.spec.SelectorSeparator.SelectorListSeparator

object SelectorEmitter {

	def emit(selector: NormalizedSelector)(implicit builder: StringBuilder): StringBuilder = selector match {
		case simple: SimpleSelector => simple match {
			case element: Element =>
				emitElement(element)
			case element: PseudoElement =>
				emitPseudoElement(element)
			case attribute: Attribute =>
				emitAttribute(attribute)
			case Id(name) =>
				builder.append("#" + name)
			case Class(name) =>
				builder.append("." + name)
			case pseudoClass: PseudoClass =>
				emitPseudoClass(pseudoClass)
		}
		case compound: Compound =>
			emitCompound(compound)
		case complex: Complex =>
			emitComplex(complex)
		case selectorList: SelectorList =>
			emitSelectorList(selectorList)
	}

	private def emitElement(element: Element)(implicit builder: StringBuilder): StringBuilder = {
		val withNamespace = element.element.namespace match {
			case Some(namespace) => builder.append((namespace match {
				case NamedNamespace(name) => name
				case AnyNamespace => "*"
				case NoNamespace => ""
			}) + "|")
			case None => builder
		}
		withNamespace.append(element.element.element.name)
	}

	private def emitPseudoElement(element: PseudoElement)(implicit builder: StringBuilder): StringBuilder =
		builder.append("::" + element.element.name)

	private def emitAttribute(attribute: Attribute)(implicit builder: StringBuilder): StringBuilder =
		builder.append("[" + (attribute.name.namespace match {
			case NamedNamespace(name) => name + "|"
			case AnyNamespace => "*|"
			case NoNamespace => ""
		}) + attribute.name.name + (attribute.target match {
			case Some(matchTarget) => matchTarget.matcher.symbol + matchTarget.value
			case None => ""
		}) + (attribute.modifier match {
			case Some(modifier) => " " + modifier.name
			case None => ""
		}) + "]")

	private def emitPseudoClass(pseudoClass: PseudoClass)(implicit builder: StringBuilder): StringBuilder =
		pseudoClass match {
			case NonFunctional(nonFunctional) =>
				builder.append(":" + nonFunctional.name)
			case Dir(directionality) =>
				builder.append(":" + PseudoClasses.Dir.name + "(" + directionality.value + ")")
			case Drop(filter) =>
				builder.append(":" + PseudoClasses.Drop.name + "(" + filter.map(_.value).mkString(" ") + ")")
			case Lang(ranges) =>
				builder.append(":" + PseudoClasses.Lang.name + "(" + ranges.map(_.value).mkString(", ") + ")")
			case SubSelector(kind, subSelector) =>
				emit(subSelector)(builder.append(":" + kind.name + "(")).append(")")
			case Nth(kind, ab, of) =>
				val base = builder.append(":" + kind.name + "(" + (ab match {
					case AnPlusB(2, 1) => "odd" // Shorter than "2n+1"
					// Assuming a and b aren't both zero
					case AnPlusB(a, 0) => a.toString + "n" // "2n" is shorter than "even"
					case AnPlusB(0, b) => b.toString
					case AnPlusB(a, b) => a.toString + "n" + (if (b < 0) "" else "+") + b.toString
				}))
				val withOf = of match {
					case Some(selector) => emit(selector)(base.append(" of "))
					case None => base
				}
				withOf.append(")")
		}


	private def emitCompound(compound: Compound)(implicit builder: StringBuilder): StringBuilder = {
		val withElement = compound.element match {
			case Some(el) => emitElement(el)(builder)
			case None => builder
		}
		val withSubClassSelectors = chainEmit(compound.subClassSelectors)(withElement)
		val withPseudoElement = compound.pseudoElement match {
			case Some(pseudoEl) => emitPseudoElement(pseudoEl)(withSubClassSelectors)
			case None => withSubClassSelectors
		}
		chainEmit(compound.furtherPseudoClasses)(withPseudoElement)
	}

	private def emitComplex(complex: Complex)(implicit builder: StringBuilder): StringBuilder =
		emit(complex.right)(emit(complex.left)(builder).append(complex.combinator.emit))


	private def emitSelectorList(list: SelectorList)(implicit builder: StringBuilder): StringBuilder =
		// Assuming there are always at least two elements
		list.selectors.tail.foldLeft(emit(list.selectors.head)){
			case (intermediateBuilder, ruleHead) => emit(ruleHead)(
				intermediateBuilder.append(SelectorListSeparator.symbol + " ")
			)
		}


	private def chainEmit(ruleHeads: Iterable[NormalizedSelector])(implicit builder: StringBuilder): StringBuilder =
		ruleHeads.foldLeft(builder){
			case (intermediateBuilder, ruleHead) => emit(ruleHead)(intermediateBuilder)
		}
}
