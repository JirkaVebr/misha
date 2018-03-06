package com.preprocessor.spec

object HtmlElements {

	/**
		* @see https://html.spec.whatwg.org/multipage/
		*/
	sealed trait HtmlElement {
		def name: String
	}

	sealed trait DocumentElement extends HtmlElement
	case object Html extends DocumentElement { def name = "html" }


	sealed trait Metadata extends HtmlElement
	case object Base extends Metadata { def name = "base" }
	case object Head extends Metadata { def name = "head" }
	case object Link extends Metadata { def name = "link" }
	case object Meta extends Metadata { def name = "meta" }
	case object Style extends Metadata { def name = "style" }
	case object Title extends Metadata { def name = "title" }


	sealed trait SectionElement extends HtmlElement
	case object Address extends SectionElement { def name = "address" }
	case object Article extends SectionElement { def name = "article" }
	case object Aside extends SectionElement { def name = "aside" }
	case object Body extends SectionElement { def name = "body" }
	case object Footer extends SectionElement { def name = "footer" }
	case object Hgroup extends SectionElement { def name = "hgroup" }
	case object Header extends SectionElement { def name = "header" }
	case object Nav extends SectionElement { def name = "nav" }
	case object Section extends SectionElement { def name = "section" }

	sealed trait Heading extends SectionElement
	case object H1 extends Heading { def name = "h1" }
	case object H2 extends Heading { def name = "h2" }
	case object H3 extends Heading { def name = "h3" }
	case object H4 extends Heading { def name = "h4" }
	case object H5 extends Heading { def name = "h5" }
	case object H6 extends Heading { def name = "h6" }


	sealed trait GroupingContent extends HtmlElement
	case object Blockquote extends GroupingContent { def name = "blockquote" }
	case object Dd extends GroupingContent { def name = "dd" }
	case object Div extends GroupingContent { def name = "div" }
	case object Dl extends GroupingContent { def name = "dl" }
	case object Dt extends GroupingContent { def name = "dt" }
	case object Figcaption extends GroupingContent { def name = "figcaption" }
	case object Figure extends GroupingContent { def name = "figure" }
	case object Hr extends GroupingContent { def name = "hr" }
	case object Li extends GroupingContent { def name = "li" }
	case object Main extends GroupingContent { def name = "main" }
	case object Menu extends GroupingContent { def name = "menu" }
	case object Ol extends GroupingContent { def name = "ol" }
	case object P extends GroupingContent { def name = "p" }
	case object Pre extends GroupingContent { def name = "pre" }
	case object Ul extends GroupingContent { def name = "ul" }


	sealed trait TextLevel extends HtmlElement
	case object A extends TextLevel { def name = "a" }
	case object Abbr extends TextLevel { def name = "abbr" }
	case object B extends TextLevel { def name = "b" }
	case object Bdi extends TextLevel { def name = "bdi" }
	case object Bdo extends TextLevel { def name = "bdo" }
	case object Br extends TextLevel { def name = "br" }
	case object Cite extends TextLevel { def name = "cite" }
	case object Code extends TextLevel { def name = "code" }
	case object Data extends TextLevel { def name = "data" }
	case object Dfn extends TextLevel { def name = "dfn" }
	case object Em extends TextLevel { def name = "em" }
	case object I extends TextLevel { def name = "i" }
	case object Kbd extends TextLevel { def name = "kbd" }
	case object Mark extends TextLevel { def name = "mark" }
	case object Q extends TextLevel { def name = "q" }
	case object Rp extends TextLevel { def name = "rp" }
	case object Rt extends TextLevel { def name = "rt" }
	case object Ruby extends TextLevel { def name = "ruby" }
	case object S extends TextLevel { def name = "s" }
	case object Samp extends TextLevel { def name = "samp" }
	case object Small extends TextLevel { def name = "small" }
	case object Span extends TextLevel { def name = "span" }
	case object Strong extends TextLevel { def name = "strong" }
	case object Sub extends TextLevel { def name = "sub" }
	case object Sup extends TextLevel { def name = "sup" }
	case object Time extends TextLevel { def name = "time" }
	case object U extends TextLevel { def name = "u" }
	case object Var extends TextLevel { def name = "var" }
	case object Wbr extends TextLevel { def name = "wbr" }


	sealed trait Edit extends HtmlElement
	case object Del extends Edit { def name = "del" }
	case object Ins extends Edit { def name = "ins" }


	sealed trait EmbeddedContent extends HtmlElement
	case object Area extends EmbeddedContent { def name = "area" }
	case object Audio extends EmbeddedContent { def name = "audio" }
	case object Embed extends EmbeddedContent { def name = "embed" }
	case object Iframe extends EmbeddedContent { def name = "iframe" }
	case object Img extends EmbeddedContent { def name = "img" }
	case object Map extends EmbeddedContent { def name = "map" }
	case object Object extends EmbeddedContent { def name = "object" }
	case object Param extends EmbeddedContent { def name = "param" }
	case object Picture extends EmbeddedContent { def name = "picture" }
	case object Source extends EmbeddedContent { def name = "source" }
	case object Track extends EmbeddedContent { def name = "track" }
	case object Video extends EmbeddedContent { def name = "video" }


	sealed trait Tabular extends HtmlElement
	case object Caption extends Tabular { def name = "caption" }
	case object Col extends Tabular { def name = "col" }
	case object Colgroup extends Tabular { def name = "colgroup" }
	case object Table extends Tabular { def name = "table" }
	case object Tbody extends Tabular { def name = "tbody" }
	case object Td extends Tabular { def name = "td" }
	case object Tfoot extends Tabular { def name = "tfoot" }
	case object Th extends Tabular { def name = "th" }
	case object Thead extends Tabular { def name = "thead" }
	case object Tr extends Tabular { def name = "tr" }


	sealed trait Forms extends HtmlElement
	case object Button extends Forms { def name = "button" }
	case object Datalist extends Forms { def name = "datalist" }
	case object Fieldset extends Forms { def name = "fieldset" }
	case object Form extends Forms { def name = "form" }
	case object Input extends Forms { def name = "input" }
	case object Label extends Forms { def name = "label" }
	case object Legend extends Forms { def name = "legend" }
	case object Meter extends Forms { def name = "meter" }
	case object Optgroup extends Forms { def name = "optgroup" }
	case object Option extends Forms { def name = "option" }
	case object Output extends Forms { def name = "output" }
	case object Progress extends Forms { def name = "progress" }
	case object Select extends Forms { def name = "select" }
	case object Textarea extends Forms { def name = "textarea" }


	sealed trait Interactive extends HtmlElement
	case object Details extends Interactive { def name = "details" }
	case object Summary extends Interactive { def name = "summary" }


	sealed trait Scripting extends HtmlElement
	case object Canvas extends Scripting { def name = "canvas" }
	case object Noscript extends Scripting { def name = "noscript" }
	case object Script extends Scripting { def name = "script" }
	case object Slot extends Scripting { def name = "slot" }
	case object Template extends Scripting { def name = "template" }


	case class CustomElement(name: String) extends HtmlElement

}
