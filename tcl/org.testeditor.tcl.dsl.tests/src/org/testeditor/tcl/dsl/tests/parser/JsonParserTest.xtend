package org.testeditor.tcl.dsl.tests.parser

import javax.inject.Inject
import org.junit.Test
import org.testeditor.tcl.JsonArray
import org.testeditor.tcl.JsonBoolean
import org.testeditor.tcl.JsonNull
import org.testeditor.tcl.JsonNumber
import org.testeditor.tcl.JsonObject
import org.testeditor.tcl.JsonString
import org.testeditor.tcl.dsl.services.TclGrammarAccess

class JsonParserTest extends AbstractParserTest {

	@Inject TclGrammarAccess grammarAccess

	private def JsonObject parseJson(CharSequence input) {
		return parserHelper.parse(input.toString.trim, grammarAccess.jsonObjectRule, JsonObject)
	}

	@Test
	def void parseEmptyJsonObject() {
		// given
		val json = '''
			{}
		'''

		// when + then
		json.parseJson => [
			members.assertEmpty
		]
	}

	@Test
	def void parseJsonWithSimpleValues() {
		// given
		val json = '''
			{
				"a" : "value1",
				"b" : true,
				"c" : false,
				"d": null,
				"e": 2e+6,
				"f": -3.14159265359
			}
		'''

		// when + then
		json.parseJson => [
			members => [
				assertSize(6)
				get(0) => [
					key.assertEquals("a")
					value.assertInstanceOf(JsonString).value.assertEquals("value1")
				]
				get(1) => [
					key.assertEquals("b")
					value.assertInstanceOf(JsonBoolean).value.assertTrue
				]
				get(2) => [
					key.assertEquals("c")
					value.assertInstanceOf(JsonBoolean).value.assertFalse
				]
				get(3) => [
					key.assertEquals("d")
					value.assertInstanceOf(JsonNull)
				]
				get(4) => [
					key.assertEquals("e")
					value.assertInstanceOf(JsonNumber).value.assertEquals("2e+6")
				]
				get(5) => [
					key.assertEquals("f")
					value.assertInstanceOf(JsonNumber).value.assertEquals("-3.14159265359")
				]
			]
		]
	}

	@Test
	def void parseJsonWithArray() {
		// given
		val json = '''
			{
				"a" : ["r", "g", "b"]
			}
		'''

		// when + then
		json.parseJson.members.assertSingleElement => [
			value.assertInstanceOf(JsonArray).values => [
				assertSize(3)
				get(0).assertInstanceOf(JsonString).value.assertEquals("r")
				get(1).assertInstanceOf(JsonString).value.assertEquals("g")
				get(2).assertInstanceOf(JsonString).value.assertEquals("b")
			]
		]
	}

	@Test
	def void parseNestedObject() {
		// given
		val json = '''
			{
				"a" : {
					"b": null,
					"c": true
				}
			}
		'''

		// when + then
		json.parseJson.members.assertSingleElement => [
			value.assertInstanceOf(JsonObject) => [
				members => [
					assertSize(2)
					get(0) => [
						key.assertEquals("b")
						value.assertInstanceOf(JsonNull)
					]
					get(1) => [
						key.assertEquals("c")
						value.assertInstanceOf(JsonBoolean).value.assertTrue
					]
				]
			]
		]
	}

}
