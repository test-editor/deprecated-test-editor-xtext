package org.testeditor.aml.dsl.tests

import com.google.inject.Injector
import javax.inject.Inject
import org.eclipse.xtext.junit4.AbstractXtextTests
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.junit.runner.RunWith
import org.testeditor.aml.dsl.AmlInjectorProvider
import org.junit.BeforeClass
import org.testeditor.aml.model.ModelPackage
import org.eclipse.xtext.junit4.validation.ValidationTestHelper

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
	
	@BeforeClass
	static def void registerEPackages() {
		ModelPackage.eINSTANCE.toString
	}
	
}