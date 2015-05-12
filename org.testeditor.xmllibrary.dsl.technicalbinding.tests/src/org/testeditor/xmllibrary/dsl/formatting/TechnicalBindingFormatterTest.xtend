package org.testeditor.xmllibrary.dsl.formatting

import org.junit.Test
import org.testeditor.xmllibrary.dsl.tests.AbstractFormatterTest

class TechnicalBindingFormatterTest extends AbstractFormatterTest {

	@Test
	def void testEmptyBinding() {
		// Given
		val input1 = '''
		binding1 label "my binding 1" {
		}'''
		val input2 = '''binding1 label "my binding 1" {}'''

		// When
		val result1 = input1.format
		val result2 = input2.format

		// Then
		assertEquals(input1, result1)
		assertEquals(input1, result2)
	}

	@Test
	def void formatMultipleBindings() {
		// Given
		val input = '''
			binding1 label "my binding1" {}
			binding2 label "my binding2" {}
			binding3 label "my binding3" {}
		'''
		val expected = '''
		binding1 label "my binding1" {
		}
		binding2 label "my binding2" {
		}
		binding3 label "my binding3" {
		}'''

		// When
		val formatted = input.format

		// Then
		assertEquals(expected, formatted)
	}

}