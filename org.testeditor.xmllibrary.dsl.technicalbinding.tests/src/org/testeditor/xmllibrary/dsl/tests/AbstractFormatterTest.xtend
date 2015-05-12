package org.testeditor.xmllibrary.dsl.tests

import javax.inject.Inject
import org.eclipse.xtext.formatting.INodeModelFormatter
import org.junit.BeforeClass
import org.testeditor.xmllibrary.model.ModelPackage

abstract class AbstractFormatterTest extends AbstractTechnicalBindingTest {
	
	@Inject protected INodeModelFormatter formatter
	
	@BeforeClass
	static def void staticSetup() {
		ModelPackage.eINSTANCE.toString
	}
	
	def String format(CharSequence source) {
		val rootNode = source.toString.rootNode
		val region = formatter.format(rootNode, 0, rootNode.length)
		return region.formattedText
	}
	
}