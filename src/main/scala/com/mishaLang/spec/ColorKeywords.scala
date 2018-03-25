package com.mishaLang.spec

import com.mishaLang.ast.Language.Value._


/**
	* If you want to add another color, make sure the key is lowercase.
	*
	* @see https://drafts.csswg.org/css-color/#colorunits
	*/
object ColorKeywords {

	lazy final val Colors: Map[java.lang.String, Color] = Map(
		"currentcolor" -> CurrentColor,
		"transparent" -> Transparent,

		"aliceblue" -> Rgba(240, 248, 255),
		"antiquewhite" -> Rgba(250, 235, 215),
		"aqua" -> Rgba(0, 255, 255),
		"aquamarine" -> Rgba(127, 255, 212),
		"azure" -> Rgba(240, 255, 255),
		"beige" -> Rgba(245, 245, 220),
		"bisque" -> Rgba(255, 228, 196),
		"black" -> Rgba(0, 0, 0),
		"blanchedalmond" -> Rgba(255, 235, 205),
		"blue" -> Rgba(0, 0, 255),
		"blueviolet" -> Rgba(138, 43, 226),
		"brown" -> Rgba(165, 42, 42),
		"burlywood" -> Rgba(222, 184, 135),
		"cadetblue" -> Rgba(95, 158, 160),
		"chartreuse" -> Rgba(127, 255, 0),
		"chocolate" -> Rgba(210, 105, 30),
		"coral" -> Rgba(255, 127, 80),
		"cornflowerblue" -> Rgba(100, 149, 237),
		"cornsilk" -> Rgba(255, 248, 220),
		"crimson" -> Rgba(220, 20, 60),
		"cyan" -> Rgba(0, 255, 255),
		"darkblue" -> Rgba(0, 0, 139),
		"darkcyan" -> Rgba(0, 139, 139),
		"darkgoldenrod" -> Rgba(184, 134, 11),
		"darkgray" -> Rgba(169, 169, 169),
		"darkgreen" -> Rgba(0, 100, 0),
		"darkgrey" -> Rgba(169, 169, 169),
		"darkkhaki" -> Rgba(189, 183, 107),
		"darkmagenta" -> Rgba(139, 0, 139),
		"darkolivegreen" -> Rgba(85, 107, 47),
		"darkorange" -> Rgba(255, 140, 0),
		"darkorchid" -> Rgba(153, 50, 204),
		"darkred" -> Rgba(139, 0, 0),
		"darksalmon" -> Rgba(233, 150, 122),
		"darkseagreen" -> Rgba(143, 188, 143),
		"darkslateblue" -> Rgba(72, 61, 139),
		"darkslategray" -> Rgba(47, 79, 79),
		"darkslategrey" -> Rgba(47, 79, 79),
		"darkturquoise" -> Rgba(0, 206, 209),
		"darkviolet" -> Rgba(148, 0, 211),
		"deeppink" -> Rgba(255, 20, 147),
		"deepskyblue" -> Rgba(0, 191, 255),
		"dimgray" -> Rgba(105, 105, 105),
		"dimgrey" -> Rgba(105, 105, 105),
		"dodgerblue" -> Rgba(30, 144, 255),
		"firebrick" -> Rgba(178, 34, 34),
		"floralwhite" -> Rgba(255, 250, 240),
		"forestgreen" -> Rgba(34, 139, 34),
		"fuchsia" -> Rgba(255, 0, 255),
		"gainsboro" -> Rgba(220, 220, 220),
		"ghostwhite" -> Rgba(248, 248, 255),
		"gold" -> Rgba(255, 215, 0),
		"goldenrod" -> Rgba(218, 165, 32),
		"gray" -> Rgba(128, 128, 128),
		"green" -> Rgba(0, 128, 0),
		"greenyellow" -> Rgba(173, 255, 47),
		"grey" -> Rgba(128, 128, 128),
		"honeydew" -> Rgba(240, 255, 240),
		"hotpink" -> Rgba(255, 105, 180),
		"indianred" -> Rgba(205, 92, 92),
		"indigo" -> Rgba(75, 0, 130),
		"ivory" -> Rgba(255, 255, 240),
		"khaki" -> Rgba(240, 230, 140),
		"lavender" -> Rgba(230, 230, 250),
		"lavenderblush" -> Rgba(255, 240, 245),
		"lawngreen" -> Rgba(124, 252, 0),
		"lemonchiffon" -> Rgba(255, 250, 205),
		"lightblue" -> Rgba(173, 216, 230),
		"lightcoral" -> Rgba(240, 128, 128),
		"lightcyan" -> Rgba(224, 255, 255),
		"lightgoldenrodyellow" -> Rgba(250, 250, 210),
		"lightgray" -> Rgba(211, 211, 211),
		"lightgreen" -> Rgba(144, 238, 144),
		"lightgrey" -> Rgba(211, 211, 211),
		"lightpink" -> Rgba(255, 182, 193),
		"lightsalmon" -> Rgba(255, 160, 122),
		"lightseagreen" -> Rgba(32, 178, 170),
		"lightskyblue" -> Rgba(135, 206, 250),
		"lightslategray" -> Rgba(119, 136, 153),
		"lightslategrey" -> Rgba(119, 136, 153),
		"lightsteelblue" -> Rgba(176, 196, 222),
		"lightyellow" -> Rgba(255, 255, 224),
		"lime" -> Rgba(0, 255, 0),
		"limegreen" -> Rgba(50, 205, 50),
		"linen" -> Rgba(250, 240, 230),
		"magenta" -> Rgba(255, 0, 255),
		"maroon" -> Rgba(128, 0, 0),
		"mediumaquamarine" -> Rgba(102, 205, 170),
		"mediumblue" -> Rgba(0, 0, 205),
		"mediumorchid" -> Rgba(186, 85, 211),
		"mediumpurple" -> Rgba(147, 112, 219),
		"mediumseagreen" -> Rgba(60, 179, 113),
		"mediumslateblue" -> Rgba(123, 104, 238),
		"mediumspringgreen" -> Rgba(0, 250, 154),
		"mediumturquoise" -> Rgba(72, 209, 204),
		"mediumvioletred" -> Rgba(199, 21, 133),
		"midnightblue" -> Rgba(25, 25, 112),
		"mintcream" -> Rgba(245, 255, 250),
		"mistyrose" -> Rgba(255, 228, 225),
		"moccasin" -> Rgba(255, 228, 181),
		"navajowhite" -> Rgba(255, 222, 173),
		"navy" -> Rgba(0, 0, 128),
		"oldlace" -> Rgba(253, 245, 230),
		"olive" -> Rgba(128, 128, 0),
		"olivedrab" -> Rgba(107, 142, 35),
		"orange" -> Rgba(255, 165, 0),
		"orangered" -> Rgba(255, 69, 0),
		"orchid" -> Rgba(218, 112, 214),
		"palegoldenrod" -> Rgba(238, 232, 170),
		"palegreen" -> Rgba(152, 251, 152),
		"paleturquoise" -> Rgba(175, 238, 238),
		"palevioletred" -> Rgba(219, 112, 147),
		"papayawhip" -> Rgba(255, 239, 213),
		"peachpuff" -> Rgba(255, 218, 185),
		"peru" -> Rgba(205, 133, 63),
		"pink" -> Rgba(255, 192, 203),
		"plum" -> Rgba(221, 160, 221),
		"powderblue" -> Rgba(176, 224, 230),
		"purple" -> Rgba(128, 0, 128),
		"rebeccapurple" -> Rgba(102, 51, 153),
		"red" -> Rgba(255, 0, 0),
		"rosybrown" -> Rgba(188, 143, 143),
		"royalblue" -> Rgba(65, 105, 225),
		"saddlebrown" -> Rgba(139, 69, 19),
		"salmon" -> Rgba(250, 128, 114),
		"sandybrown" -> Rgba(244, 164, 96),
		"seagreen" -> Rgba(46, 139, 87),
		"seashell" -> Rgba(255, 245, 238),
		"sienna" -> Rgba(160, 82, 45),
		"silver" -> Rgba(192, 192, 192),
		"skyblue" -> Rgba(135, 206, 235),
		"slateblue" -> Rgba(106, 90, 205),
		"slategray" -> Rgba(112, 128, 144),
		"slategrey" -> Rgba(112, 128, 144),
		"snow" -> Rgba(255, 250, 250),
		"springgreen" -> Rgba(0, 255, 127),
		"steelblue" -> Rgba(70, 130, 180),
		"tan" -> Rgba(210, 180, 140),
		"teal" -> Rgba(0, 128, 128),
		"thistle" -> Rgba(216, 191, 216),
		"tomato" -> Rgba(255, 99, 71),
		"turquoise" -> Rgba(64, 224, 208),
		"violet" -> Rgba(238, 130, 238),
		"wheat" -> Rgba(245, 222, 179),
		"white" -> Rgba(255, 255, 255),
		"whitesmoke" -> Rgba(245, 245, 245),
		"yellow" -> Rgba(255, 255, 0),
		"yellowgreen" -> Rgba(154, 205, 50)
	)
}
