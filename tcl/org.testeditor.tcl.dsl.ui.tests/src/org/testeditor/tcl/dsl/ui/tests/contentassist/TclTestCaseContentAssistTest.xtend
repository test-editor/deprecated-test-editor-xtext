package org.testeditor.tcl.dsl.ui.tests.contentassist

import java.io.InputStream
import javax.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.resource.FileExtensionProvider
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.util.StringInputStream
import org.eclipse.xtext.xbase.junit.ui.AbstractContentAssistTest
import org.junit.Assert
import org.junit.Before
import org.junit.Ignore
import org.junit.Test
import org.junit.runner.RunWith
import org.testeditor.tcl.dsl.ui.tests.TclUiInjectorProvider

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(TclUiInjectorProvider))
class TclTestCaseContentAssistTest extends AbstractContentAssistTest {

	@Inject FileExtensionProvider fileExtensionProvider
	@Inject XtextResourceSet resourceSet

	@Before
	def void initialize() {
	}

	override getResourceFor(InputStream stream) {
		getResourceFor(stream, "src/test/Test." + fileExtensionProvider.primaryFileExtension)
	}

	def XtextResource getResourceFor(InputStream stream, String fileName) {
		val result = resourceSet.createResource(URI::createURI(fileName))
		result.load(stream, null)
		result as XtextResource
	}

	@Test
	@Ignore
	def void validateStringArray() {
		val parseResult = getResourceFor(new StringInputStream('''
			package test
			# Target
			
			* my spec test description
		'''), "src/test/Target.tsl").parseResult

		Assert.assertFalse(parseResult.hasSyntaxErrors)

		newBuilder.append('''
			package test
			# Test implements Target
			
			Mask: SomeMask
		''').assertTextAtCursorPosition("Mask", "* my spec test description")

	}

	@Test
	def void testEmptyStepWithoutSpec() {
		newBuilder.append('''
			package test
			# Test
			
			Mask: SomeMask
		''').assertTextAtCursorPosition("Mask", "* ", "implements")
	}

	@Test
	def void testEmptyStepWithSpec() {
		newBuilder.append('''
			package test
			# Test implements Target
			
			Mask: SomeMask
		''').assertTextAtCursorPosition("Mask", "* ")
	}

}
