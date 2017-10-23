package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Test
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.junit.Before
import org.eclipse.emf.ecore.resource.Resource

class TclJsonUtilTest  extends AbstractTclTest {

	@Inject TclJsonUtil jsonUtil // class under test
	
	@Inject JvmTypeReferenceUtil typeReferenceUtil
	
	@Before def void setupTypeReferenceUtil() {
		typeReferenceUtil.initWith(null as Resource)
	}
	
	@Test 
	def void testJsonArrayTypeDetection() {
		// given
		val jsonArrayType = typeReferenceUtil.jsonArrayJvmTypeReference
		
		// when
		val isJsonType = jsonUtil.isJsonType(jsonArrayType)
		
		// then
		isJsonType.assertTrue		
	}

	@Test 
	def void testStringNotJsonTypeDetection() {
		// given
		val stringType = typeReferenceUtil.stringJvmTypeReference
		
		// when
		val isJsonType = jsonUtil.isJsonType(stringType)
		
		// then
		isJsonType.assertFalse		
	}

}