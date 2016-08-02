package org.testeditor.dsl.common.testing

import com.google.inject.Provider
import javax.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.XtextResourceSet
import org.testeditor.aml.AmlModel
import org.testeditor.tcl.TclModel
import org.testeditor.tsl.TslModel
import org.eclipse.xtend.lib.annotations.Accessors

@Accessors
class ResourceSetHelper {
	@Inject public Provider<XtextResourceSet> resourceSetProvider
	@Inject public XtextResourceSet resourceSet
	
	def void setUpResourceSet(Object resourceContext) {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = resourceContext		
	}
		
	def <T extends EObject> T addToResourceSet(T model) {
		switch (model) {
			TclModel: {
				if (model.macroCollection !== null && model.macroCollection.macros.size > 0) {
					return model.addToResourceSet(model.macroCollection.name+".tml")
				} else if (model.test !== null )  {
					return model.addToResourceSet(model.test.name+".tcl")
				} else if (model.config !== null ) {
					return model.addToResourceSet(model.config.name+".config")
				} else {
					throw new RuntimeException('''tcl model is neither a macroCollection nor a test nor a config''')
				}
			}
			AmlModel:
				return model.addToResourceSet("Dummy.aml")
			TslModel:
				return model.addToResourceSet("Dummy.tsl")
			default:
				throw new RuntimeException('''unknown model='«model.class.name»'.''')
		}
	}

	def <T extends EObject> T addToResourceSet(T model, String fileName) {
		val uri = URI.createURI(fileName)

		val newResource = resourceSet.createResource(uri)
		newResource.contents.add(model)
		return model
	}
	
}