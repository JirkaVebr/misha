package com.preprocessor.spec

object HtmlElements {

	/**
		* @see https://html.spec.whatwg.org/multipage/
		*/
	sealed trait HtmlElement {
		def name: String
	}

	case object AnyElement extends HtmlElement { override def name = "*" }

	sealed trait DocumentElement extends HtmlElement
	case object Html extends DocumentElement { override def name = "html" }


	sealed trait Metadata extends HtmlElement
	case object Base extends Metadata { override def name = "base" }
	case object Head extends Metadata { override def name = "head" }
	case object Link extends Metadata { override def name = "link" }
	case object Meta extends Metadata { override def name = "meta" }
	case object Style extends Metadata { override def name = "style" }
	case object Title extends Metadata { override def name = "title" }


	sealed trait SectionElement extends HtmlElement
	case object Address extends SectionElement { override def name = "address" }
	case object Article extends SectionElement { override def name = "article" }
	case object Aside extends SectionElement { override def name = "aside" }
	case object Body extends SectionElement { override def name = "body" }
	case object Footer extends SectionElement { override def name = "footer" }
	case object Hgroup extends SectionElement { override def name = "hgroup" }
	case object Header extends SectionElement { override def name = "header" }
	case object Nav extends SectionElement { override def name = "nav" }
	case object Section extends SectionElement { override def name = "section" }

	sealed trait Heading extends SectionElement
	case object H1 extends Heading { override def name = "h1" }
	case object H2 extends Heading { override def name = "h2" }
	case object H3 extends Heading { override def name = "h3" }
	case object H4 extends Heading { override def name = "h4" }
	case object H5 extends Heading { override def name = "h5" }
	case object H6 extends Heading { override def name = "h6" }


	sealed trait GroupingContent extends HtmlElement
	case object Blockquote extends GroupingContent { override def name = "blockquote" }
	case object Dd extends GroupingContent { override def name = "dd" }
	case object Div extends GroupingContent { override def name = "div" }
	case object Dl extends GroupingContent { override def name = "dl" }
	case object Dt extends GroupingContent { override def name = "dt" }
	case object Figcaption extends GroupingContent { override def name = "figcaption" }
	case object Figure extends GroupingContent { override def name = "figure" }
	case object Hr extends GroupingContent { override def name = "hr" }
	case object Li extends GroupingContent { override def name = "li" }
	case object Main extends GroupingContent { override def name = "main" }
	case object Menu extends GroupingContent { override def name = "menu" }
	case object Ol extends GroupingContent { override def name = "ol" }
	case object P extends GroupingContent { override def name = "p" }
	case object Pre extends GroupingContent { override def name = "pre" }
	case object Ul extends GroupingContent { override def name = "ul" }


	sealed trait TextLevel extends HtmlElement
	case object A extends TextLevel { override def name = "a" }
	case object Abbr extends TextLevel { override def name = "abbr" }
	case object B extends TextLevel { override def name = "b" }
	case object Bdi extends TextLevel { override def name = "bdi" }
	case object Bdo extends TextLevel { override def name = "bdo" }
	case object Br extends TextLevel { override def name = "br" }
	case object Cite extends TextLevel { override def name = "cite" }
	case object Code extends TextLevel { override def name = "code" }
	case object Data extends TextLevel { override def name = "data" }
	case object Dfn extends TextLevel { override def name = "dfn" }
	case object Em extends TextLevel { override def name = "em" }
	case object I extends TextLevel { override def name = "i" }
	case object Kbd extends TextLevel { override def name = "kbd" }
	case object Mark extends TextLevel { override def name = "mark" }
	case object Q extends TextLevel { override def name = "q" }
	case object Rp extends TextLevel { override def name = "rp" }
	case object Rt extends TextLevel { override def name = "rt" }
	case object Ruby extends TextLevel { override def name = "ruby" }
	case object S extends TextLevel { override def name = "s" }
	case object Samp extends TextLevel { override def name = "samp" }
	case object Small extends TextLevel { override def name = "small" }
	case object Span extends TextLevel { override def name = "span" }
	case object Strong extends TextLevel { override def name = "strong" }
	case object Sub extends TextLevel { override def name = "sub" }
	case object Sup extends TextLevel { override def name = "sup" }
	case object Time extends TextLevel { override def name = "time" }
	case object U extends TextLevel { override def name = "u" }
	case object Var extends TextLevel { override def name = "var" }
	case object Wbr extends TextLevel { override def name = "wbr" }


	sealed trait Edit extends HtmlElement
	case object Del extends Edit { override def name = "del" }
	case object Ins extends Edit { override def name = "ins" }


	sealed trait EmbeddedContent extends HtmlElement
	case object Area extends EmbeddedContent { override def name = "area" }
	case object Audio extends EmbeddedContent { override def name = "audio" }
	case object Embed extends EmbeddedContent { override def name = "embed" }
	case object Iframe extends EmbeddedContent { override def name = "iframe" }
	case object Img extends EmbeddedContent { override def name = "img" }
	case object Map extends EmbeddedContent { override def name = "map" }
	case object Object extends EmbeddedContent { override def name = "object" }
	case object Param extends EmbeddedContent { override def name = "param" }
	case object Picture extends EmbeddedContent { override def name = "picture" }
	case object Source extends EmbeddedContent { override def name = "source" }
	case object Track extends EmbeddedContent { override def name = "track" }
	case object Video extends EmbeddedContent { override def name = "video" }


	sealed trait Tabular extends HtmlElement
	case object Caption extends Tabular { override def name = "caption" }
	case object Col extends Tabular { override def name = "col" }
	case object Colgroup extends Tabular { override def name = "colgroup" }
	case object Table extends Tabular { override def name = "table" }
	case object Tbody extends Tabular { override def name = "tbody" }
	case object Td extends Tabular { override def name = "td" }
	case object Tfoot extends Tabular { override def name = "tfoot" }
	case object Th extends Tabular { override def name = "th" }
	case object Thead extends Tabular { override def name = "thead" }
	case object Tr extends Tabular { override def name = "tr" }


	sealed trait Forms extends HtmlElement
	case object Button extends Forms { override def name = "button" }
	case object Datalist extends Forms { override def name = "datalist" }
	case object Fieldset extends Forms { override def name = "fieldset" }
	case object Form extends Forms { override def name = "form" }
	case object Input extends Forms { override def name = "input" }
	case object Label extends Forms { override def name = "label" }
	case object Legend extends Forms { override def name = "legend" }
	case object Meter extends Forms { override def name = "meter" }
	case object Optgroup extends Forms { override def name = "optgroup" }
	case object Option extends Forms { override def name = "option" }
	case object Output extends Forms { override def name = "output" }
	case object Progress extends Forms { override def name = "progress" }
	case object Select extends Forms { override def name = "select" }
	case object Textarea extends Forms { override def name = "textarea" }


	sealed trait Interactive extends HtmlElement
	case object Details extends Interactive { override def name = "details" }
	case object Summary extends Interactive { override def name = "summary" }


	sealed trait Scripting extends HtmlElement
	case object Canvas extends Scripting { override def name = "canvas" }
	case object Noscript extends Scripting { override def name = "noscript" }
	case object Script extends Scripting { override def name = "script" }
	case object Slot extends Scripting { override def name = "slot" }
	case object Template extends Scripting { override def name = "template" }


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
