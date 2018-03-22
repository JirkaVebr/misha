package com.mishaLang.spec

object HtmlElements {

	/**
		* @see https://html.spec.whatwg.org/multipage/
		*/
	sealed trait HtmlElement {
		val name: String
	}

	case object AnyElement extends HtmlElement { override val name = "*" }

	sealed trait DocumentElement extends HtmlElement
	case object Html extends DocumentElement { override val name = "html" }


	sealed trait Metadata extends HtmlElement
	case object Base extends Metadata { override val name = "base" }
	case object Head extends Metadata { override val name = "head" }
	case object Link extends Metadata { override val name = "link" }
	case object Meta extends Metadata { override val name = "meta" }
	case object Style extends Metadata { override val name = "style" }
	case object Title extends Metadata { override val name = "title" }


	sealed trait SectionElement extends HtmlElement
	case object Address extends SectionElement { override val name = "address" }
	case object Article extends SectionElement { override val name = "article" }
	case object Aside extends SectionElement { override val name = "aside" }
	case object Body extends SectionElement { override val name = "body" }
	case object Footer extends SectionElement { override val name = "footer" }
	case object Hgroup extends SectionElement { override val name = "hgroup" }
	case object Header extends SectionElement { override val name = "header" }
	case object Nav extends SectionElement { override val name = "nav" }
	case object Section extends SectionElement { override val name = "section" }

	sealed trait Heading extends SectionElement
	case object H1 extends Heading { override val name = "h1" }
	case object H2 extends Heading { override val name = "h2" }
	case object H3 extends Heading { override val name = "h3" }
	case object H4 extends Heading { override val name = "h4" }
	case object H5 extends Heading { override val name = "h5" }
	case object H6 extends Heading { override val name = "h6" }


	sealed trait GroupingContent extends HtmlElement
	case object Blockquote extends GroupingContent { override val name = "blockquote" }
	case object Dd extends GroupingContent { override val name = "dd" }
	case object Div extends GroupingContent { override val name = "div" }
	case object Dl extends GroupingContent { override val name = "dl" }
	case object Dt extends GroupingContent { override val name = "dt" }
	case object Figcaption extends GroupingContent { override val name = "figcaption" }
	case object Figure extends GroupingContent { override val name = "figure" }
	case object Hr extends GroupingContent { override val name = "hr" }
	case object Li extends GroupingContent { override val name = "li" }
	case object Main extends GroupingContent { override val name = "main" }
	case object Menu extends GroupingContent { override val name = "menu" }
	case object Ol extends GroupingContent { override val name = "ol" }
	case object P extends GroupingContent { override val name = "p" }
	case object Pre extends GroupingContent { override val name = "pre" }
	case object Ul extends GroupingContent { override val name = "ul" }


	sealed trait TextLevel extends HtmlElement
	case object A extends TextLevel { override val name = "a" }
	case object Abbr extends TextLevel { override val name = "abbr" }
	case object B extends TextLevel { override val name = "b" }
	case object Bdi extends TextLevel { override val name = "bdi" }
	case object Bdo extends TextLevel { override val name = "bdo" }
	case object Br extends TextLevel { override val name = "br" }
	case object Cite extends TextLevel { override val name = "cite" }
	case object Code extends TextLevel { override val name = "code" }
	case object Data extends TextLevel { override val name = "data" }
	case object Dfn extends TextLevel { override val name = "dfn" }
	case object Em extends TextLevel { override val name = "em" }
	case object I extends TextLevel { override val name = "i" }
	case object Kbd extends TextLevel { override val name = "kbd" }
	case object Mark extends TextLevel { override val name = "mark" }
	case object Q extends TextLevel { override val name = "q" }
	case object Rp extends TextLevel { override val name = "rp" }
	case object Rt extends TextLevel { override val name = "rt" }
	case object Ruby extends TextLevel { override val name = "ruby" }
	case object S extends TextLevel { override val name = "s" }
	case object Samp extends TextLevel { override val name = "samp" }
	case object Small extends TextLevel { override val name = "small" }
	case object Span extends TextLevel { override val name = "span" }
	case object Strong extends TextLevel { override val name = "strong" }
	case object Sub extends TextLevel { override val name = "sub" }
	case object Sup extends TextLevel { override val name = "sup" }
	case object Time extends TextLevel { override val name = "time" }
	case object U extends TextLevel { override val name = "u" }
	case object Var extends TextLevel { override val name = "var" }
	case object Wbr extends TextLevel { override val name = "wbr" }


	sealed trait Edit extends HtmlElement
	case object Del extends Edit { override val name = "del" }
	case object Ins extends Edit { override val name = "ins" }


	sealed trait EmbeddedContent extends HtmlElement
	case object Area extends EmbeddedContent { override val name = "area" }
	case object Audio extends EmbeddedContent { override val name = "audio" }
	case object Embed extends EmbeddedContent { override val name = "embed" }
	case object Iframe extends EmbeddedContent { override val name = "iframe" }
	case object Img extends EmbeddedContent { override val name = "img" }
	case object Map extends EmbeddedContent { override val name = "map" }
	case object Object extends EmbeddedContent { override val name = "object" }
	case object Param extends EmbeddedContent { override val name = "param" }
	case object Picture extends EmbeddedContent { override val name = "picture" }
	case object Source extends EmbeddedContent { override val name = "source" }
	case object Track extends EmbeddedContent { override val name = "track" }
	case object Video extends EmbeddedContent { override val name = "video" }


	sealed trait Tabular extends HtmlElement
	case object Caption extends Tabular { override val name = "caption" }
	case object Col extends Tabular { override val name = "col" }
	case object Colgroup extends Tabular { override val name = "colgroup" }
	case object Table extends Tabular { override val name = "table" }
	case object Tbody extends Tabular { override val name = "tbody" }
	case object Td extends Tabular { override val name = "td" }
	case object Tfoot extends Tabular { override val name = "tfoot" }
	case object Th extends Tabular { override val name = "th" }
	case object Thead extends Tabular { override val name = "thead" }
	case object Tr extends Tabular { override val name = "tr" }


	sealed trait Forms extends HtmlElement
	case object Button extends Forms { override val name = "button" }
	case object Datalist extends Forms { override val name = "datalist" }
	case object Fieldset extends Forms { override val name = "fieldset" }
	case object Form extends Forms { override val name = "form" }
	case object Input extends Forms { override val name = "input" }
	case object Label extends Forms { override val name = "label" }
	case object Legend extends Forms { override val name = "legend" }
	case object Meter extends Forms { override val name = "meter" }
	case object Optgroup extends Forms { override val name = "optgroup" }
	case object Option extends Forms { override val name = "option" }
	case object Output extends Forms { override val name = "output" }
	case object Progress extends Forms { override val name = "progress" }
	case object Select extends Forms { override val name = "select" }
	case object Textarea extends Forms { override val name = "textarea" }


	sealed trait Interactive extends HtmlElement
	case object Details extends Interactive { override val name = "details" }
	case object Summary extends Interactive { override val name = "summary" }


	sealed trait Scripting extends HtmlElement
	case object Canvas extends Scripting { override val name = "canvas" }
	case object Noscript extends Scripting { override val name = "noscript" }
	case object Script extends Scripting { override val name = "script" }
	case object Slot extends Scripting { override val name = "slot" }
	case object Template extends Scripting { override val name = "template" }


	case class CustomElement(name: String) extends HtmlElement


	val htmlElements: Map[String, HtmlElement] = scala.collection.immutable.Map(
		A.name -> A,
		Abbr.name -> Abbr,
		Address.name -> Address,
		AnyElement.name -> AnyElement,
		Area.name -> Area,
		Article.name -> Article,
		Aside.name -> Aside,
		Audio.name -> Audio,
		B.name -> B,
		Base.name -> Base,
		Bdi.name -> Bdi,
		Bdo.name -> Bdo,
		Blockquote.name -> Blockquote,
		Body.name -> Body,
		Br.name -> Br,
		Button.name -> Button,
		Canvas.name -> Canvas,
		Caption.name -> Caption,
		Cite.name -> Cite,
		Code.name -> Code,
		Col.name -> Col,
		Colgroup.name -> Colgroup,
		Data.name -> Data,
		Datalist.name -> Datalist,
		Dd.name -> Dd,
		Del.name -> Del,
		Details.name -> Details,
		Dfn.name -> Dfn,
		Div.name -> Div,
		Dl.name -> Dl,
		Dt.name -> Dt,
		Em.name -> Em,
		Embed.name -> Embed,
		Fieldset.name -> Fieldset,
		Figcaption.name -> Figcaption,
		Figure.name -> Figure,
		Footer.name -> Footer,
		Form.name -> Form,
		H1.name -> H1,
		H2.name -> H2,
		H3.name -> H3,
		H4.name -> H4,
		H5.name -> H5,
		H6.name -> H6,
		Head.name -> Head,
		Header.name -> Header,
		Hgroup.name -> Hgroup,
		Hr.name -> Hr,
		Html.name -> Html,
		I.name -> I,
		Iframe.name -> Iframe,
		Img.name -> Img,
		Input.name -> Input,
		Ins.name -> Ins,
		Kbd.name -> Kbd,
		Label.name -> Label,
		Legend.name -> Legend,
		Li.name -> Li,
		Link.name -> Link,
		Main.name -> Main,
		Map.name -> Map,
		Mark.name -> Mark,
		Menu.name -> Menu,
		Meta.name -> Meta,
		Meter.name -> Meter,
		Nav.name -> Nav,
		Noscript.name -> Noscript,
		Object.name -> Object,
		Ol.name -> Ol,
		Optgroup.name -> Optgroup,
		Option.name -> Option,
		Output.name -> Output,
		P.name -> P,
		Param.name -> Param,
		Picture.name -> Picture,
		Pre.name -> Pre,
		Progress.name -> Progress,
		Q.name -> Q,
		Rp.name -> Rp,
		Rt.name -> Rt,
		Ruby.name -> Ruby,
		S.name -> S,
		Samp.name -> Samp,
		Script.name -> Script,
		Section.name -> Section,
		Select.name -> Select,
		Slot.name -> Slot,
		Small.name -> Small,
		Source.name -> Source,
		Span.name -> Span,
		Strong.name -> Strong,
		Style.name -> Style,
		Sub.name -> Sub,
		Summary.name -> Summary,
		Sup.name -> Sup,
		Table.name -> Table,
		Tbody.name -> Tbody,
		Td.name -> Td,
		Template.name -> Template,
		Textarea.name -> Textarea,
		Tfoot.name -> Tfoot,
		Th.name -> Th,
		Thead.name -> Thead,
		Time.name -> Time,
		Title.name -> Title,
		Tr.name -> Tr,
		Track.name -> Track,
		U.name -> U,
		Ul.name -> Ul,
		Var.name -> Var,
		Video.name -> Video,
		Wbr.name -> Wbr
	)
}
