package com.mishaLang.integration

class RuleSpec extends BaseIntegrationSpec {

	behavior of "Rule facilities in Misha"

	they should "correctly evaluate simple functions" in {
		assert(run(
			"""@let $color = ($color: Color) =>
				|	@property("color", $color)
				|.foo
				|	@let $width = ($length: Length) =>
				|		@property("width", $length)
				|	color blue
				|	width 123px
				|""".stripMargin) ===
			""".foo {
				|	color: #00f;
				|	width: 123px;
				|}
				|""".stripMargin)
	}


	they should "correctly evaluate nested rules" in {
		assert(run(
			"""@let $color = ($color: Color) =>
				|	@property("color", $color)
				|@let $width = ($length: Length) =>
				|	@property("width", $length)
				|@let $border-radius = ($radius: Length) =>
				|	@property("border-radius", $radius)
				|.foo
				|	color blue
				|	& .bar
				|		border-radius .2em
				|	width 123px
				|""".stripMargin) ===
			""".foo {
				|	color: #00f;
				|	width: 123px;
				|}
				|.foo .bar {
				|	border-radius: 0.2em;
				|}
				|""".stripMargin)
	}


	they should "correctly evaluate repeated applications of the same mixin" in {
		assert(run(
			"""@let $color = ($color: Color) =>
				|	@property("color", $color)
				|@let $width = ($length: Length) =>
				|	@property("width", $length)
				|@let $button = ($c: Color) =>
				|	color $c
				|	width 100px
				|	& span
				|		color black
				|.foo
				|	$button(red)
				|.bar
				|	$button(blue)
				|""".stripMargin) ===
			""".foo {
				|	color: #f00;
				|	width: 100px;
				|}
				|.foo span {
				|	color: #000;
				|}
				|.bar {
				|	color: #00f;
				|	width: 100px;
				|}
				|.bar span {
				|	color: #000;
				|}
				|""".stripMargin)
	}

}
