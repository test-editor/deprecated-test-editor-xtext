package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTest
import javax.inject.Inject

class TclJvmModelInferrerTest extends AbstractTest {

	@Inject TclJvmModelInferrer modelInferrer

	@Test
	def void testRemoveFileExtension() {
		// expect
		modelInferrer.removeFileExtension('test').assertEquals('test')
		modelInferrer.removeFileExtension('test.tcl').assertEquals('test')
		modelInferrer.removeFileExtension('test.xyz.tcl').assertEquals('test.xyz')
	}

}