package org.testeditor.tcl.dsl.tests

import javax.inject.Inject
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.impl.TclFactoryImpl
import org.testeditor.tml.TestStepContext
import org.testeditor.tml.dsl.tests.TmlModelGenerator

class TclModelGenerator extends TmlModelGenerator {
	@Inject TclFactoryImpl tclFactory

	def TclModel tclModel() {
		return tclFactory.createTclModel
	}

	def TclModel withTestCase(TclModel me, TestCase testCase) {
		me.test = testCase
		return me
	}

	def TestCase testCase(String name) {
		return tclFactory.createTestCase => [it.name = name]
	}

	def TclModel withEnvVariable(TclModel me, String ... names) {
		names.forEach [ varName |
			me.environmentVariableReferences.add(tclFactory.createEnvironmentVariableReference => [name = varName])
		]
		return me
	}

	def TclModel withPackage(TclModel me, String packageName) {
		me.^package = packageName
		return me
	}

	def TclModel withNamespaceImport(TclModel me, String namespace) {
		if (me.importSection == null) {
			me.importSection = xtypeFactory.createXImportSection
		}
		me.importSection.importDeclarations += xtypeFactory.createXImportDeclaration => [
			it.importedNamespace = namespace
		]
		return me
	}

	def TestCase withSpecificationStep(TestCase me, SpecificationStepImplementation step) {
		me.steps.add(step)
		return me
	}

	def SpecificationStepImplementation specificationStep(String ... texts) {
		return tclFactory.createSpecificationStepImplementation => [
			texts.forEach[text|contents.add(tslFactory.createStepContentText => [value = text])]
		]
	}

	def SpecificationStepImplementation withTestStepContext(SpecificationStepImplementation me,
		TestStepContext testStepContext) {
		me.contexts += testStepContext
		return me
	}

}
