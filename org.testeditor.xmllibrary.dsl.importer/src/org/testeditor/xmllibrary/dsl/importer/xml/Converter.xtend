package org.testeditor.xmllibrary.dsl.importer.xml

import com.google.inject.Guice
import com.google.inject.Module
import javax.inject.Inject
import javax.inject.Singleton
import org.eclipse.xtext.serializer.ISerializer
import org.testeditor.xmllibrary.domain.action.ActionGroups
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes
import org.testeditor.xmllibrary.dsl.TechnicalBindingDslRuntimeModule

@Singleton
class Converter {
	
	@Inject ActionGroupsConverter actionGroupsConverter
	@Inject TechnicalBindingConverter technicalBindingConverter
	
	def dispatch String convert(ActionGroups actionGroups) {
		// TODO can we use proxies here?
		val newActionGroups = actionGroupsConverter.convert(actionGroups, null)	
		println(newActionGroups)
		throw new UnsupportedOperationException
	}
	
	def dispatch String convert(TechnicalBindingTypes bindings) {
		val newBindings = technicalBindingConverter.convert(bindings)
		val x = technicalBindingSerializer.serialize(newBindings)
		println(x)
		return x
	}
	
	def dispatch String convert(Object object) {
		// TODO error
		return "// ERROR!"
	}
	
	def ISerializer getTechnicalBindingSerializer() {
		val module = new TechnicalBindingDslRuntimeModule as Module
		val injector = Guice.createInjector(module)
		return injector.getInstance(ISerializer)
	}
	
}