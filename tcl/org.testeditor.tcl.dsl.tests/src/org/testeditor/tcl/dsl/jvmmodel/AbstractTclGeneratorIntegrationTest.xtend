package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import javax.inject.Provider
import org.eclipse.xtext.generator.IFileSystemAccess
import org.eclipse.xtext.generator.IGenerator
import org.eclipse.xtext.generator.InMemoryFileSystemAccess
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.aml.model.AmlModel
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.AbstractTest

abstract class AbstractTclGeneratorIntegrationTest extends AbstractTest {

	@Inject
	protected Provider<XtextResourceSet> resourceSetProvider

	@Inject
	protected XtextResourceSet resourceSet

	protected ParseHelper<AmlModel> amlParseHelper

	@Inject
	protected ParseHelper<TclModel> tclParseHelper
	
	@Inject
	protected ValidationTestHelper validationHelper

	@Inject
	protected IGenerator generator

	protected InMemoryFileSystemAccess fsa

	@Before
	def void setup() {
		resourceSet = resourceSetProvider.get
		val injector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		amlParseHelper = injector.getInstance(ParseHelper)
		fsa = new InMemoryFileSystemAccess
	}

	protected def String generate(TclModel model) {
		generator.doGenerate(model.eResource, fsa)
		val file = fsa.getJavaFile(model.package, model.name)
		return file.toString
	}

	protected def getJavaFile(InMemoryFileSystemAccess fsa, String ^package, String name) {
		val key = '''«IFileSystemAccess.DEFAULT_OUTPUT»«package.replaceAll('\\.', '/')»/«name».java'''
		return fsa.allFiles.get(key)
	}

}