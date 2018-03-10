package com.preprocessor.spec

import com.preprocessor.spec.PseudoClasses.NonFunctional.NonFunctionalPseudoClass

object PseudoClasses {

	sealed trait PseudoClass {
		val name: String
	}

	case object Lang extends PseudoClass {
		override val name: String = "lang"
	}


	sealed trait SubSelector extends PseudoClass

	case object Not extends SubSelector {
		override val name: String = "not"
	}
	case object Matches extends SubSelector {
		override val name: String = "matches"
	}
	case object Something extends SubSelector {
		override val name: String = "something"
	}
	case object Has extends SubSelector {
		override val name: String = "has"
	}
	case object Current extends SubSelector {
		override val name: String = "current"
	}

	val subSelectors: Map[String, SubSelector] = Map(
		Not.name -> Not,
		Matches.name -> Matches,
		Something.name -> Something,
		Has.name -> Has,
		Current.name -> Current
	)


	trait ArgumentEnum {
		def value: String
	}


	/**
		* @see https://drafts.csswg.org/selectors-4/#the-dir-pseudo
		*/
	case object Dir extends PseudoClass {
		override val name: String = "dir"
	}

	sealed trait Directionality extends ArgumentEnum

	case object Ltr extends Directionality {
		override val value: String = "ltr"
	}
	case object Rtl extends Directionality {
		override val value: String = "rtl"
	}
	case class UndefinedDirectionality(value: String) extends Directionality

	val directionality: Map[String, Directionality] = Map(
		Ltr.value -> Ltr,
		Rtl.value -> Rtl
	)


	/**
		* @see https://drafts.csswg.org/selectors-4/#drag-pseudos
		*/
	case object Drop extends PseudoClass {
		override val name: String = "drop"
	}

	sealed trait DropFilter extends ArgumentEnum

	case object Active extends DropFilter {
		override val value: String = "active"
	}
	case object Valid extends DropFilter {
		override val value: String = "valid"
	}
	case object Invalid extends DropFilter {
		override val value: String = "invalid"
	}


	/**
		* @see https://drafts.csswg.org/selectors-4/#child-index
		*/
	sealed trait Nth extends PseudoClass

	case object Child extends Nth {
		override val name: String = "nth-child"
	}
	case object LastChild extends Nth {
		override val name: String = "nth-last-child"
	}
	case object OfType extends Nth {
		override val name: String = "nth-of-type"
	}
	case object LastOfType extends Nth {
		override val name: String = "nth-last-of-type"
	}
	case object Col extends Nth {
		override val name: String = "nth-col"
	}
	case object LastCol extends Nth {
		override val name: String = "nth-last-col"
	}

	val nthPseudoClasses: Map[String, Nth] = Map(
		Child.name -> Child,
		LastChild.name -> LastChild,
		OfType.name -> OfType,
		LastOfType.name -> LastOfType,
		Col.name -> Col,
		LastCol.name -> LastCol
	)

	/**
		* No need for special cases for `even` and `odd` as these reduce to 2n and 2n+1 respectively
		* @see https://drafts.csswg.org/css-syntax-3/#anb-microsyntax
		*/
	case class AnPlusB(a: Int, b: Int)


	object NonFunctional {
		sealed trait NonFunctionalPseudoClass extends PseudoClass

		/**
			* Drop and Current are intentionally essentially duplicates of their special-support counterparts in order to ease
			* handling of e.g. `:drop` as well as `:drop(active)`
			*
			* Enumerating them in order to allow use of parboiled2's valueMaps and to make subsequent AST manipulation easier.
			*/
		case object Active extends NonFunctionalPseudoClass { override val name: String = "active" }
		case object AnyLink extends NonFunctionalPseudoClass { override val name: String = "any-link" }
		case object Blank extends NonFunctionalPseudoClass { override val name: String = "blank" }
		case object Checked extends NonFunctionalPseudoClass { override val name: String = "checked" }
		case object Current extends NonFunctionalPseudoClass { override val name: String = "link" }
		case object Default extends NonFunctionalPseudoClass { override val name: String = "default" }
		case object Disabled extends NonFunctionalPseudoClass { override val name: String = "disabled" }
		case object Drop extends NonFunctionalPseudoClass { override val name: String = "link" }
		case object Empty extends NonFunctionalPseudoClass { override val name: String = "empty" }
		case object Enabled extends NonFunctionalPseudoClass { override val name: String = "enabled" }
		case object FirstChild extends NonFunctionalPseudoClass { override val name: String = "first-child" }
		case object FirstOfType extends NonFunctionalPseudoClass { override val name: String = "first-of-type" }
		case object Focus extends NonFunctionalPseudoClass { override val name: String = "focus" }
		case object FocusVisible extends NonFunctionalPseudoClass { override val name: String = "focus-visible" }
		case object FocusWithin extends NonFunctionalPseudoClass { override val name: String = "focus-within" }
		case object Future extends NonFunctionalPseudoClass { override val name: String = "future" }
		case object Hover extends NonFunctionalPseudoClass { override val name: String = "hover" }
		case object Indeterminate extends NonFunctionalPseudoClass { override val name: String = "indeterminate" }
		case object InRange extends NonFunctionalPseudoClass { override val name: String = "in-range" }
		case object Invalid extends NonFunctionalPseudoClass { override val name: String = "invalid" }
		case object LastChild extends NonFunctionalPseudoClass { override val name: String = "last-child" }
		case object LastOfType extends NonFunctionalPseudoClass { override val name: String = "last-of-type" }
		case object Link extends NonFunctionalPseudoClass { override val name: String = "link" }
		case object LocalLink extends NonFunctionalPseudoClass { override val name: String = "local-link" }
		case object OnlyChild extends NonFunctionalPseudoClass { override val name: String = "only-child" }
		case object OnlyOfType extends NonFunctionalPseudoClass { override val name: String = "only-of-type" }
		case object OutOfRange extends NonFunctionalPseudoClass { override val name: String = "out-of-range" }
		case object Past extends NonFunctionalPseudoClass { override val name: String = "past" }
		case object PlaceholderShown extends NonFunctionalPseudoClass { override val name: String = "placeholder-shown" }
		case object ReadOnly extends NonFunctionalPseudoClass { override val name: String = "read-only" }
		case object ReadWrite extends NonFunctionalPseudoClass { override val name: String = "read-write" }
		case object Required extends NonFunctionalPseudoClass { override val name: String = "required" }
		case object Root extends NonFunctionalPseudoClass { override val name: String = "root" }
		case object Scope extends NonFunctionalPseudoClass { override val name: String = "scope" }
		case object Target extends NonFunctionalPseudoClass { override val name: String = "target" }
		case object TargetWithin extends NonFunctionalPseudoClass { override val name: String = "target-within" }
		case object UserInvalid extends NonFunctionalPseudoClass { override val name: String = "user-invalid" }
		case object Valid extends NonFunctionalPseudoClass { override val name: String = "valid" }
		case object Visited extends NonFunctionalPseudoClass { override val name: String = "visited" }

		case class CustomPseudoClass(name: String) extends NonFunctionalPseudoClass
	}

	val nonFunctionalPseudoClass: Map[String, NonFunctionalPseudoClass] = Map(
		NonFunctional.Active.name -> NonFunctional.Active,
		NonFunctional.AnyLink.name -> NonFunctional.AnyLink,
		NonFunctional.Blank.name -> NonFunctional.Blank,
		NonFunctional.Checked.name -> NonFunctional.Checked,
		NonFunctional.Current.name -> NonFunctional.Current,
		NonFunctional.Default.name -> NonFunctional.Default,
		NonFunctional.Disabled.name -> NonFunctional.Disabled,
		NonFunctional.Drop.name -> NonFunctional.Drop,
		NonFunctional.Empty.name -> NonFunctional.Empty,
		NonFunctional.Enabled.name -> NonFunctional.Enabled,
		NonFunctional.FirstChild.name -> NonFunctional.FirstChild,
		NonFunctional.FirstOfType.name -> NonFunctional.FirstOfType,
		NonFunctional.Focus.name -> NonFunctional.Focus,
		NonFunctional.FocusVisible.name -> NonFunctional.FocusVisible,
		NonFunctional.FocusWithin.name -> NonFunctional.FocusWithin,
		NonFunctional.Future.name -> NonFunctional.Future,
		NonFunctional.Hover.name -> NonFunctional.Hover,
		NonFunctional.Indeterminate.name -> NonFunctional.Indeterminate,
		NonFunctional.InRange.name -> NonFunctional.InRange,
		NonFunctional.Invalid.name -> NonFunctional.Invalid,
		NonFunctional.LastChild.name -> NonFunctional.LastChild,
		NonFunctional.LastOfType.name -> NonFunctional.LastOfType,
		NonFunctional.Link.name -> NonFunctional.Link,
		NonFunctional.LocalLink.name -> NonFunctional.LocalLink,
		NonFunctional.OnlyChild.name -> NonFunctional.OnlyChild,
		NonFunctional.OnlyOfType.name -> NonFunctional.OnlyOfType,
		NonFunctional.OutOfRange.name -> NonFunctional.OutOfRange,
		NonFunctional.Past.name -> NonFunctional.Past,
		NonFunctional.PlaceholderShown.name -> NonFunctional.PlaceholderShown,
		NonFunctional.ReadOnly.name -> NonFunctional.ReadOnly,
		NonFunctional.ReadWrite.name -> NonFunctional.ReadWrite,
		NonFunctional.Required.name -> NonFunctional.Required,
		NonFunctional.Root.name -> NonFunctional.Root,
		NonFunctional.Scope.name -> NonFunctional.Scope,
		NonFunctional.Target.name -> NonFunctional.Target,
		NonFunctional.TargetWithin.name -> NonFunctional.TargetWithin,
		NonFunctional.UserInvalid.name -> NonFunctional.UserInvalid,
		NonFunctional.Valid.name -> NonFunctional.Valid,
		NonFunctional.Visited.name -> NonFunctional.Visited
	)


}
