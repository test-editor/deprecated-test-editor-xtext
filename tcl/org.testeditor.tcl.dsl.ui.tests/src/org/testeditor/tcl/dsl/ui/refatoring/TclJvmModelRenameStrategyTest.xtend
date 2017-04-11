package org.testeditor.tcl.dsl.ui.refatoring

import javax.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.testing.util.ParseHelper
import org.junit.Before
import org.junit.Test
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.ui.tests.AbstractUiTest

class TclJvmModelRenameStrategyTest extends AbstractUiTest {

	@Inject TclJvmModelRenameStrategy renameStrategy
	@Inject ParseHelper<TclModel> parseHelper
	@Inject XtextResourceSet resourceSet

	TclModel tclModel

	@Before
	def void parseModel() {
		val input = '''
			package com.example
			
			# MyTest
			
			* step 1
		'''
		val uri = URI.createPlatformResourceURI("/example/src/test/java/example/Test.tcl", false)
		tclModel = parseHelper.parse(input, uri, resourceSet)
	}

	@Test
	def void pathToRenameIsRetrievedForTestCase() {
		// given
		val elementUri = EcoreUtil.getURI(tclModel.test)

		// when
		val pathToRename = renameStrategy.getPathToRename(elementUri, resourceSet)

		// then
		pathToRename.toString.assertEquals("/example/src/test/java/example/Test.tcl")
	}

	@Test
	def void pathToRenameIsNullForSpecificationStep() {
		// given
		val elementUri = EcoreUtil.getURI(tclModel.test.steps.head)

		// when
		val pathToRename = renameStrategy.getPathToRename(elementUri, resourceSet)

		// then
		pathToRename.assertNull
	}

}
