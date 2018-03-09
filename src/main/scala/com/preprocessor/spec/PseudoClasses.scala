package com.preprocessor.spec

import com.preprocessor.spec.PseudoClasses.NonFunctional.NonFunctionalPseudoClass

object PseudoClasses {

	sealed trait PseudoClass {
		def name: String
	}

	sealed trait SubSelector extends PseudoClass

	case object Not extends SubSelector {
		override def name: String = "not"
	}
	case object Matches extends SubSelector {
		override def name: String = "matches"
	}
	case object Something extends SubSelector {
		override def name: String = "something"
	}
	case object Has extends SubSelector {
		override def name: String = "has"
	}
	case object Current extends SubSelector {
		override def name: String = "current"
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
	sealed trait Directionality extends ArgumentEnum

	case object Ltr extends Directionality {
		override def value: String = "ltr"
	}
	case object Rtl extends Directionality {
		override def value: String = "rtl"
	}
	case class UndefinedDirectionality(value: String) extends Directionality

	val directionality: Map[String, Directionality] = Map(
		Ltr.value -> Ltr,
		Rtl.value -> Rtl
	)


	/**
		* @see https://drafts.csswg.org/selectors-4/#drag-pseudos
		*/
	sealed trait DropFilter extends ArgumentEnum

	case object Active extends DropFilter {
		override def value: String = "active"
	}
	case object Valid extends DropFilter {
		override def value: String = "valid"
	}
	case object Invalid extends DropFilter {
		override def value: String = "invalid"
	}


	/**
		* @see https://drafts.csswg.org/selectors-4/#child-index
		*/
	sealed trait Nth extends PseudoClass {
		def kind: String

		override def name: String = "nth-" + kind
	}

	case object Child extends Nth {
		override def kind: String = "child"
	}
	case object LastChild extends Nth {
		override def kind: String = "last-child"
	}
	case object OfType extends Nth {
		override def kind: String = "of-type"
	}
	case object LastOfType extends Nth {
		override def kind: String = "last-of-type"
	}
	case object Col extends Nth {
		override def kind: String = "col"
	}
	case object LastCol extends Nth {
		override def kind: String = "last-col"
	}

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
		case object Active extends NonFunctionalPseudoClass { override def name: String = "active" }
		case object AnyLink extends NonFunctionalPseudoClass { override def name: String = "any-link" }
		case object Blank extends NonFunctionalPseudoClass { override def name: String = "blank" }
		case object Checked extends NonFunctionalPseudoClass { override def name: String = "checked" }
		case object Current extends NonFunctionalPseudoClass { override def name: String = "link" }
		case object Default extends NonFunctionalPseudoClass { override def name: String = "default" }
		case object Disabled extends NonFunctionalPseudoClass { override def name: String = "disabled" }
		case object Drop extends NonFunctionalPseudoClass { override def name: String = "link" }
		case object Empty extends NonFunctionalPseudoClass { override def name: String = "empty" }
		case object Enabled extends NonFunctionalPseudoClass { override def name: String = "enabled" }
		case object FirstChild extends NonFunctionalPseudoClass { override def name: String = "first-child" }
		case object FirstOfType extends NonFunctionalPseudoClass { override def name: String = "first-of-type" }
		case object Focus extends NonFunctionalPseudoClass { override def name: String = "focus" }
		case object FocusVisible extends NonFunctionalPseudoClass { override def name: String = "focus-visible" }
		case object FocusWithin extends NonFunctionalPseudoClass { override def name: String = "focus-within" }
		case object Future extends NonFunctionalPseudoClass { override def name: String = "future" }
		case object Hover extends NonFunctionalPseudoClass { override def name: String = "hover" }
		case object Indeterminate extends NonFunctionalPseudoClass { override def name: String = "indeterminate" }
		case object InRange extends NonFunctionalPseudoClass { override def name: String = "in-range" }
		case object Invalid extends NonFunctionalPseudoClass { override def name: String = "invalid" }
		case object LastChild extends NonFunctionalPseudoClass { override def name: String = "last-child" }
		case object LastOfType extends NonFunctionalPseudoClass { override def name: String = "last-of-type" }
		case object Link extends NonFunctionalPseudoClass { override def name: String = "link" }
		case object LocalLink extends NonFunctionalPseudoClass { override def name: String = "local-link" }
		case object OnlyChild extends NonFunctionalPseudoClass { override def name: String = "only-child" }
		case object OnlyOfType extends NonFunctionalPseudoClass { override def name: String = "only-of-type" }
		case object OutOfRange extends NonFunctionalPseudoClass { override def name: String = "out-of-range" }
		case object Past extends NonFunctionalPseudoClass { override def name: String = "past" }
		case object PlaceholderShown extends NonFunctionalPseudoClass { override def name: String = "placeholder-shown" }
		case object ReadOnly extends NonFunctionalPseudoClass { override def name: String = "read-only" }
		case object ReadWrite extends NonFunctionalPseudoClass { override def name: String = "read-write" }
		case object Required extends NonFunctionalPseudoClass { override def name: String = "required" }
		case object Root extends NonFunctionalPseudoClass { override def name: String = "root" }
		case object Scope extends NonFunctionalPseudoClass { override def name: String = "scope" }
		case object Target extends NonFunctionalPseudoClass { override def name: String = "target" }
		case object TargetWithin extends NonFunctionalPseudoClass { override def name: String = "target-within" }
		case object UserInvalid extends NonFunctionalPseudoClass { override def name: String = "user-invalid" }
		case object Valid extends NonFunctionalPseudoClass { override def name: String = "valid" }
		case object Visited extends NonFunctionalPseudoClass { override def name: String = "visited" }

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
