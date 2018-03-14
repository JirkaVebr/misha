package com.preprocessor.emitter

import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace, NoNamespace}
import com.preprocessor.ast.RuleContext.{AtRule, RuleSelector}
import com.preprocessor.ast.Selector
import com.preprocessor.ast.Selector._
import com.preprocessor.ast.Symbol.RuleContextSymbol
import com.preprocessor.spec.PseudoClasses

object RuleHeadEmitter {

	def emit(head: RuleContextSymbol.Value)(implicit builder: StringBuilder): StringBuilder = head match {
		case RuleSelector(selector) => emit(selector)
		case _: AtRule => sys.error("todo") // TODO
	}

	def emit(selector: Selector.NormalizedSelector)(implicit builder: StringBuilder): StringBuilder = selector match {
		case simple: SimpleSelector => simple match {
			case element: Element => emitElement(element)
			case element: PseudoElement => emitPseudoElement(element)
			case Attribute(name, target, modifier) => ???
			case Id(name) => builder.append("#" + name)
			case Class(name) => builder.append("." + name)
			case pseudoClass: PseudoClass => pseudoClass match {
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
				case Nth(kind, ab, of) => ???
			}
		}
		case Compound(element, subClassSelectors, pseudoElement, furtherPseudoClasses) =>
			???
		case Complex(combinator, left, right) =>
			emit(right)(emit(left)(builder).append(combinator.emit))
		case SelectorList(selectors) => ???
	}

	def emitElement(element: Element)(implicit builder: StringBuilder): StringBuilder =
		element.element.namespace match {
			case Some(namespace) => builder.append((namespace match {
				case NamedNamespace(name) => name
				case AnyNamespace => "*"
				case NoNamespace => ""
			}) + "|")
			case None => builder
		}

	def emitPseudoElement(element: PseudoElement)(implicit builder: StringBuilder): StringBuilder =
		builder.append("::" + element.element.name)

}
