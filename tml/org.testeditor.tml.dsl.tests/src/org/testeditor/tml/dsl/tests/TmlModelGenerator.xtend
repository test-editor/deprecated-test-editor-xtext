package org.testeditor.tml.dsl.tests

import javax.inject.Inject
import org.eclipse.xtext.xtype.XtypeFactory
import org.testeditor.aml.Component
import org.testeditor.aml.Template
import org.testeditor.aml.impl.AmlFactoryImpl
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.Macro
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.TestStep
import org.testeditor.tml.TestStepContext
import org.testeditor.tml.TmlModel
import org.testeditor.tml.impl.TmlFactoryImpl
import org.testeditor.tsl.impl.TslFactoryImpl
import org.testeditor.aml.TemplateVariable

class TmlModelGenerator {
	@Inject TmlFactoryImpl tmlFactory
	@Inject protected AmlFactoryImpl amlFactory
	@Inject protected TslFactoryImpl tslFactory
	@Inject protected XtypeFactory xtypeFactory

	def TmlModel tmlModel(String macroCollection) {
		return tmlFactory.createTmlModel => [
			name = macroCollection
		]
	}

	def TmlModel withPackage(TmlModel me, String packageName) {
		return me => [^package = packageName]
	}

	def TmlModel withImportNamespace(TmlModel me, String namespace) {
		if (me.importSection == null) {
			me.importSection = xtypeFactory.createXImportSection
		}
		me.importSection.importDeclarations += xtypeFactory.createXImportDeclaration => [
			it.importedNamespace = namespace
		]
		return me
	}

	def TmlModel withMacro(TmlModel me, Macro macro) {
		me.macros += macro
		return me
	}

	def Macro macro(String macroName) {
		return tmlFactory.createMacro => [name = macroName]
	}

	def Macro withTemplate(Macro me, Template template) {
		me.template = template
		return me
	}

	def Macro withTestStepContext(Macro me, TestStepContext testStepContext) {
		me.contexts += testStepContext
		return me
	}

	def Template template(String ... texts) {
		return amlFactory.createTemplate.withText(texts)
	}

	def Template withParameter(Template me, TemplateVariable variable) {
		me.contents += variable
		return me
	}

	def Template withParameter(Template me, String variable) {
		me.contents += amlFactory.createTemplateVariable => [name=variable]
		return me
	}

	def Template withText(Template me, String ... texts) {
		return me => [
			texts.forEach[text|contents += amlFactory.createTemplateText => [value = text]]
		]
	}

	def TestStep testStep(String ... texts) {
		return tmlFactory.createTestStep.withText(texts)
	}

	def TestStep withVariableReference(TestStep me, String variableReferenceName) {
		me.contents += tmlFactory.createStepContentVariableReference => [
			variable = amlFactory.createTemplateVariable => [
				name = variableReferenceName
			]
		]
		return me
	}

	def TestStep withParameter(TestStep me, String parameter) {
		me.contents += tslFactory.createStepContentVariable => [value = parameter]
		return me
	}

	def TestStep withText(TestStep me, String ... texts) {
		return me => [
			texts.forEach[text|contents += tslFactory.createStepContentText => [value = text]]
		]
	}

	def MacroTestStepContext macroTestStepContext(TmlModel model) {
		return tmlFactory.createMacroTestStepContext => [macroModel = model]
	}

	def MacroTestStepContext withTestStep(MacroTestStepContext me, TestStep testStep) {
		me.step = testStep
		return me
	}

	def ComponentTestStepContext componentTestStepContext(Component referencedComponent) {
		return tmlFactory.createComponentTestStepContext => [
			component = referencedComponent
		]
	}

	def ComponentTestStepContext withTestStep(ComponentTestStepContext me, TestStep testStep) {
		me.steps += testStep
		return me
	}

}
