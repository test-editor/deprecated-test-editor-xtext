package org.testeditor.xmllibrary.dsl.importer.xml

import com.google.inject.Guice
import com.google.inject.Module
import javax.inject.Inject
import javax.inject.Singleton
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.serializer.ISerializer
import org.testeditor.xmllibrary.domain.action.ActionGroups
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes
import org.testeditor.xmllibrary.dsl.AllActionGroupsDslRuntimeModule
import org.testeditor.xmllibrary.dsl.TechnicalBindingDslRuntimeModule

@Singleton
class Converter {
	
	val actionGroupInjector = Guice.createInjector(new AllActionGroupsDslRuntimeModule as Module, DummyCrossReferenceSerializer.module)
	val technicalBindingInjector = Guice.createInjector(new TechnicalBindingDslRuntimeModule as Module)
	
	@Inject ActionGroupsConverter actionGroupsConverter
	@Inject TechnicalBindingConverter technicalBindingConverter
	
	def dispatch String convert(ActionGroups actionGroups) {
		// Convert
		val newActionGroups = actionGroupsConverter.convert(actionGroups)	

		// Put in Resource		
		val resource = actionGroupInjector.getProvider(XtextResource).get
		resource.contents += newActionGroups
		
		// Serialize
		val serializer = actionGroupInjector.getInstance(ISerializer)
		val serialized = serializer.serialize(newActionGroups)
		return serialized
	}
	
	def dispatch String convert(TechnicalBindingTypes bindings) {
		// Convert
		val newBindings = technicalBindingConverter.convert(bindings)
		
		// Serialize
		val serializer = technicalBindingInjector.getInstance(ISerializer)
		val serialized = serializer.serialize(newBindings)
		return serialized
	}
	
	def dispatch String convert(Object object) {
		return "/* Something went wrong here ;-) */"
	}
	
}