package org.testeditor.aml.dsl.tests

import com.google.inject.Injector
import javax.inject.Inject
import org.eclipse.xtext.junit4.AbstractXtextTests
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.runner.RunWith
import org.testeditor.aml.dsl.AmlInjectorProvider

@InjectWith(AmlInjectorProvider)
@RunWith(XtextRunner)
abstract class AbstractTest extends AbstractXtextTests {
	
	@Inject protected extension AssertionHelper
	@Inject protected extension ValidationTestHelper
	@Inject Injector injector
	
	override setUp() throws Exception {
		super.setUp()
		setInjector(this.injector)
	}
	
}