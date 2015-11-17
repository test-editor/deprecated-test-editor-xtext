package org.testeditor.tcl.dsl.jvmmodel

import com.google.inject.Inject
import com.google.inject.Provider
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.junit.Ignore
import org.testeditor.tcl.dsl.tests.AbstractTest

@Ignore
class TclJvmModelInferrerIntegrationTest extends AbstractTest {
	
	@Inject
	private Provider<XtextResourceSet> resourceSetProvider
	
	@Before
	def void parseAmlModel() {
		
	}
	
}