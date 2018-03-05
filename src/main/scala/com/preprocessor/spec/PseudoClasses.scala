package com.preprocessor.spec

import com.preprocessor.spec.PseudoClasses.Nullary.NullaryPseudoClass

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


	object Nullary {
		sealed trait NullaryPseudoClass extends PseudoClass

		/**
			* Drop and Current are intentionally essentially duplicates of their special-support counterparts in order to ease
			* handling of e.g. `:drop` as well as `:drop(active)`
			*
			* Enumerating them in order to allow use of parboiled2's valueMaps and to make subsequent AST manipulation easier.
			*/
		case object Active extends NullaryPseudoClass { override def name: String = "active" }
		case object AnyLink extends NullaryPseudoClass { override def name: String = "any-link" }
		case object Blank extends NullaryPseudoClass { override def name: String = "blank" }
		case object Checked extends NullaryPseudoClass { override def name: String = "checked" }
		case object Current extends NullaryPseudoClass { override def name: String = "link" }
		case object Default extends NullaryPseudoClass { override def name: String = "default" }
		case object Disabled extends NullaryPseudoClass { override def name: String = "disabled" }
		case object Drop extends NullaryPseudoClass { override def name: String = "link" }
		case object Empty extends NullaryPseudoClass { override def name: String = "empty" }
		case object Enabled extends NullaryPseudoClass { override def name: String = "enabled" }
		case object FirstChild extends NullaryPseudoClass { override def name: String = "first-child" }
		case object FirstOfType extends NullaryPseudoClass { override def name: String = "first-of-type" }
		case object Focus extends NullaryPseudoClass { override def name: String = "focus" }
		case object FocusVisible extends NullaryPseudoClass { override def name: String = "focus-visible" }
		case object FocusWithin extends NullaryPseudoClass { override def name: String = "focus-within" }
		case object Future extends NullaryPseudoClass { override def name: String = "future" }
		case object Hover extends NullaryPseudoClass { override def name: String = "hover" }
		case object Indeterminate extends NullaryPseudoClass { override def name: String = "indeterminate" }
		case object InRange extends NullaryPseudoClass { override def name: String = "in-range" }
		case object Invalid extends NullaryPseudoClass { override def name: String = "invalid" }
		case object LastChild extends NullaryPseudoClass { override def name: String = "last-child" }
		case object LastOfType extends NullaryPseudoClass { override def name: String = "last-of-type" }
		case object Link extends NullaryPseudoClass { override def name: String = "link" }
		case object LocalLink extends NullaryPseudoClass { override def name: String = "local-link" }
		case object OnlyChild extends NullaryPseudoClass { override def name: String = "only-child" }
		case object OnlyOfType extends NullaryPseudoClass { override def name: String = "only-of-type" }
		case object OutOfRange extends NullaryPseudoClass { override def name: String = "out-of-range" }
		case object Past extends NullaryPseudoClass { override def name: String = "past" }
		case object PlaceholderShown extends NullaryPseudoClass { override def name: String = "placeholder-shown" }
		case object ReadOnly extends NullaryPseudoClass { override def name: String = "read-only" }
		case object ReadWrite extends NullaryPseudoClass { override def name: String = "read-write" }
		case object Required extends NullaryPseudoClass { override def name: String = "required" }
		case object Root extends NullaryPseudoClass { override def name: String = "root" }
		case object Scope extends NullaryPseudoClass { override def name: String = "scope" }
		case object Target extends NullaryPseudoClass { override def name: String = "target" }
		case object TargetWithin extends NullaryPseudoClass { override def name: String = "target-within" }
		case object UserInvalid extends NullaryPseudoClass { override def name: String = "user-invalid" }
		case object Valid extends NullaryPseudoClass { override def name: String = "valid" }
		case object Visited extends NullaryPseudoClass { override def name: String = "visited" }
	}

	val nullaryPseudoClass: Map[String, NullaryPseudoClass] = Map(
		Nullary.Active.name -> Nullary.Active,
		Nullary.AnyLink.name -> Nullary.AnyLink,
		Nullary.Blank.name -> Nullary.Blank,
		Nullary.Checked.name -> Nullary.Checked,
		Nullary.Current.name -> Nullary.Current,
		Nullary.Default.name -> Nullary.Default,
		Nullary.Disabled.name -> Nullary.Disabled,
		Nullary.Drop.name -> Nullary.Drop,
		Nullary.Empty.name -> Nullary.Empty,
		Nullary.Enabled.name -> Nullary.Enabled,
		Nullary.FirstChild.name -> Nullary.FirstChild,
		Nullary.FirstOfType.name -> Nullary.FirstOfType,
		Nullary.Focus.name -> Nullary.Focus,
		Nullary.FocusVisible.name -> Nullary.FocusVisible,
		Nullary.FocusWithin.name -> Nullary.FocusWithin,
		Nullary.Future.name -> Nullary.Future,
		Nullary.Hover.name -> Nullary.Hover,
		Nullary.Indeterminate.name -> Nullary.Indeterminate,
		Nullary.InRange.name -> Nullary.InRange,
		Nullary.Invalid.name -> Nullary.Invalid,
		Nullary.LastChild.name -> Nullary.LastChild,
		Nullary.LastOfType.name -> Nullary.LastOfType,
		Nullary.Link.name -> Nullary.Link,
		Nullary.LocalLink.name -> Nullary.LocalLink,
		Nullary.OnlyChild.name -> Nullary.OnlyChild,
		Nullary.OnlyOfType.name -> Nullary.OnlyOfType,
		Nullary.OutOfRange.name -> Nullary.OutOfRange,
		Nullary.Past.name -> Nullary.Past,
		Nullary.PlaceholderShown.name -> Nullary.PlaceholderShown,
		Nullary.ReadOnly.name -> Nullary.ReadOnly,
		Nullary.ReadWrite.name -> Nullary.ReadWrite,
		Nullary.Required.name -> Nullary.Required,
		Nullary.Root.name -> Nullary.Root,
		Nullary.Scope.name -> Nullary.Scope,
		Nullary.Target.name -> Nullary.Target,
		Nullary.TargetWithin.name -> Nullary.TargetWithin,
		Nullary.UserInvalid.name -> Nullary.UserInvalid,
		Nullary.Valid.name -> Nullary.Valid,
		Nullary.Visited.name -> Nullary.Visited
	)


}
