package org.testeditor.tcl.dsl.ui.tests.contentassist

import java.io.InputStream
import javax.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.xtext.resource.FileExtensionProvider
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.XtextRunner
import org.eclipse.xtext.xbase.junit.ui.AbstractContentAssistTest
import org.junit.Before
import org.junit.runner.RunWith
import org.testeditor.tcl.dsl.ui.tests.TclUiInjectorProvider

@RunWith(XtextRunner)
@InjectWith(TclUiInjectorProvider)
abstract class AbstractTclContentAssistTest extends AbstractContentAssistTest {

	@Inject FileExtensionProvider fileExtensionProvider
	@Inject XtextResourceSet resourceSet

	@Before
	def void initialize() {
	}

	/** ensure that AbstractContentAssistTest uses the this resourceSet  */
	override getResourceFor(InputStream stream) {
		getResourceFor(stream, "src/test/Test." + fileExtensionProvider.primaryFileExtension)
	}

	def XtextResource getResourceFor(InputStream stream, String fileName) {
		val result = resourceSet.createResource(URI::createURI(fileName))
		result.load(stream, null)
		result as XtextResource
	}
}
