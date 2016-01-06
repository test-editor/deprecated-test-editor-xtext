package org.testeditor.tsl.dsl.parser

import org.junit.Test
import org.testeditor.tsl.util.TslModelUtil
import com.google.inject.Inject
import org.testeditor.tsl.BoldText
import org.testeditor.tsl.BoldCursiveText
import org.testeditor.tsl.CursiveText

class MarkupParserTest extends AbstractParserTest {

	@Inject extension TslModelUtil

	@Test
	def void specWithStepsAndHeadlineMarkups() {
		// given
		val tsl = '''
			package packageB
			# TestSpecName
			
			some markup
			===========
			
			single underlined
			-----------------
			
			other not underlined
			
			* step1_1 "value"     /*test */ step1_2 step1_3
				// inter line 
				 * step2_1.
				*    step3_1  step3_2 
		'''

		// expect 
		tsl.parse [
			specification.steps.assertSize(3) => [
				get(0).contents.restoreString.assertEquals('step1_1 "value" step1_2 step1_3')
				get(1).contents.restoreString.assertEquals('step2_1')
				get(2).contents.restoreString.assertEquals('step3_1 step3_2')
			]
			specification.descriptions.assertSize(3) => [
				get(0) => [
					textline.elements.map[text].join(" ").assertEquals("some markup")
					doubleUnderlined.assertTrue
					underlined.assertFalse
				]
				get(1) => [
					textline.elements.map[text].join(" ").assertEquals("single underlined")
					doubleUnderlined.assertFalse
					underlined.assertTrue
				]
				get(2) => [
					textline.elements.map[text].join(" ").assertEquals("other not underlined")
					doubleUnderlined.assertFalse
					underlined.assertFalse
				]
			]
		]
	}

	@Test
	def void boldAndCursiveMarkup() {
		// given
		val tsl = '''
			package test
			
			# Test
			 
			 
			  1 + 2 = 33, +bold+ t // test				  
			  some issue				  
			    1. +important stuff+ ++cursive++ +++matter+++ that + 4 does //				    
			    2. compute
			    /*
			  ok
			  */
			* ok.  
		'''

		// expect
		tsl.parse [
			specification.steps.assertSize(1)
			specification.descriptions.assertSize(4) => [ // four markup lines
				get(0).textline.elements.assertSize(8) => [ // eight elements in first markup line
					get(6).me.assertInstanceOf(BoldText).text.head.assertEquals('bold')
				]
				get(2).textline.elements.assertSize(9) => [ // nine elements in third markup line
					get(2).me.assertInstanceOf(BoldText).text.join(" ").assertEquals('important stuff')
					get(3).me.assertInstanceOf(CursiveText).text.head.assertEquals('cursive')
					get(4).me.assertInstanceOf(BoldCursiveText).text.head.assertEquals('matter')
				]
			]
		]
	}

}
