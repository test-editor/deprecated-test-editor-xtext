package org.testeditor.aml.dsl.tests

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder
import org.eclipse.xtext.xtype.XtypeFactory
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentType
import org.testeditor.aml.InteractionType
import org.testeditor.aml.MethodReference
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.impl.AmlFactoryImpl

class AmlModelGenerator {
	@Inject AmlFactoryImpl amlFactory
	@Inject protected XtypeFactory xtypeFactory
	@Inject JvmTypeReferenceBuilder.Factory jvmTypeReferenceBuilderFactory

	def AmlModel amlModel() {
		return amlFactory.createAmlModel => [^package = "com.example"]
	}

	def AmlModel withNamespaceImport(AmlModel me, String namespace) {
		if (me.importSection == null) {
			me.importSection = xtypeFactory.createXImportSection
		}
		me.importSection.importDeclarations += xtypeFactory.createXImportDeclaration => [
			it.importedNamespace = namespace
		]
		return me
	}

	def InteractionType interactionType(String name) {
		return amlFactory.createInteractionType => [it.name = name]
	}

	def <T> MethodReference methodReference(ResourceSet resourceSet, Class<T> clazz, String methodName,
		String ... templateParameters) {
		val jvmTypeReferenceBuilder = jvmTypeReferenceBuilderFactory.create(resourceSet)
		val jvmType = jvmTypeReferenceBuilder.typeRef(clazz)
		val fixture = (jvmType.type as JvmGenericType).members.filter(JvmOperation).filter[simpleName == methodName].
			head
		if (fixture === null) {
			throw new RuntimeException('''could not find method '«methodName»' in class '«clazz.canonicalName»'.''')
		}
		return amlFactory.createMethodReference => [
			operation = fixture
			parameters += templateParameters.map[templateVariable]
			it.typeReference = jvmType as JvmParameterizedTypeReference
		]
	}

	def TemplateVariable templateVariable(String name) {
		return amlFactory.createTemplateVariable => [it.name = name]
	}

	def ComponentType componentType(String name) {
		return amlFactory.createComponentType => [it.name = name]
	}

	def Component component(String name) {
		return amlFactory.createComponent => [it.name = name]
	}

}