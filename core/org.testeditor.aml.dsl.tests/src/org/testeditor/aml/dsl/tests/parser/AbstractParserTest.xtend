package org.testeditor.aml.dsl.tests.parser

import javax.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import org.testeditor.aml.dsl.tests.AbstractTest
import org.testeditor.aml.model.AmlModel
import org.testeditor.aml.model.ModelElement

abstract class AbstractParserTest extends AbstractTest {
	
	@Inject
	protected ParseHelper<AmlModel> parser

	protected def AmlModel parse(CharSequence input) {
		return parser.parse(input)
	}
	
	/**
	 * Creates a sample model by adding a package definition to the passed input, i.e.
	 * <pre>
	 * 	package com.example
	 * 	«input»
	 * </pre>
	 * and returns the first element of the passed type within the model's eAllContents.
	 * 
	 * @param the partial model to parse
	 * @param elementClass the expected type
	 * @return {@code model.eAllContents.filter(elementClass).head}
	 */
	protected def <T extends ModelElement> T parse(CharSequence input, Class<T> elementClass) {
		val newInput = '''
			package com.example
			
			«input»
		'''
		val model = parser.parse(newInput)
		return model.eAllContents.filter(elementClass).head
	}

}