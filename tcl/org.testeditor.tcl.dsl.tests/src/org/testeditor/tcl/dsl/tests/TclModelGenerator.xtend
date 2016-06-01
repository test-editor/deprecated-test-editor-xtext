package org.testeditor.tcl.dsl.tests

import javax.inject.Inject
import org.testeditor.tcl.EnvironmentVariableReference
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.impl.TclFactoryImpl
import org.testeditor.tml.dsl.tests.TmlModelGenerator

class TclModelGenerator extends TmlModelGenerator {
	@Inject TclFactoryImpl tclFactory

	def TclModel tclModel() {
		return tclFactory.createTclModel => [^package = "com.example"]
	}

	def TestCase testCase(String name) {
		return tclFactory.createTestCase => [it.name = name]
	}

	def Iterable<EnvironmentVariableReference> envVariables(String ... names) {
		return names.map [ varName |
			tclFactory.createEnvironmentVariableReference => [name = varName]
		]
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

	def SpecificationStepImplementation specificationStep(String ... texts) {
		return tclFactory.createSpecificationStepImplementation => [
			texts.forEach[text|contents.add(tslFactory.createStepContentText => [value = text])]
		]
	}

}
