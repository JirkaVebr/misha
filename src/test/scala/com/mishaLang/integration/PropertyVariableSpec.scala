package com.mishaLang.integration

class PropertyVariableSpec extends BaseIntegrationSpec {


	behavior of "Property variables"


	they should "correctly evaluate property variables" in {
		assert(run(
			""".foo
				|	width 123px
				|	height $$width
				|""".stripMargin) ===
			""".foo {
				|	width: 123px;
				|	height: 123px;
				|}
				|""".stripMargin)
		assert(run(
			""".foo
				|	width 123px
				|.foo
				|	height $$width
				|""".stripMargin) ===
			""".foo {
				|	width: 123px;
				|	height: 123px;
				|}
				|""".stripMargin)
	}
}
