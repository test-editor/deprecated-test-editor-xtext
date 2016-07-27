/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import javax.inject.Provider
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.generator.IFileSystemAccess
import org.eclipse.xtext.generator.IGenerator
import org.eclipse.xtext.generator.InMemoryFileSystemAccess
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tsl.TslModel

abstract class AbstractTclGeneratorIntegrationTest extends AbstractTclTest {

	@Inject protected Provider<XtextResourceSet> resourceSetProvider
	@Inject protected XtextResourceSet resourceSet

	protected ParseHelper<AmlModel> amlParseHelper

	@Inject protected ParseHelper<TclModel> tclParseHelper
	@Inject protected IGenerator generator

	protected InMemoryFileSystemAccess fsa

	@Before
	def void setup() {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = this
		val injector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		amlParseHelper = injector.getInstance(ParseHelper)
		fsa = new InMemoryFileSystemAccess
	}

	protected def AmlModel parseAmlModel(String aml) {
		return amlParseHelper.parse(aml, resourceSet).assertNoSyntaxErrors
	}

	protected def TclModel parseTclModel(String tcl) {
		return tclParseHelper.parse(tcl, resourceSet).assertNoSyntaxErrors
	}

	protected def String generate(TclModel model) {
		generator.doGenerate(model.eResource, fsa)
		val file = fsa.getJavaFile(model.package, model.test.name)
		return file.toString
	}

	protected def Object getJavaFile(InMemoryFileSystemAccess fsa, String ^package, String name) {
		val key = '''«IFileSystemAccess.DEFAULT_OUTPUT»«package.replaceAll('\\.', '/')»/«name».java'''
		return fsa.allFiles.get(key)
	}

	protected def <T extends EObject> T addToResourceSet(T model) {
		switch (model) {
			TclModel: {
				if (model.macroCollection !== null && model.macroCollection.macros.size > 0) {
					return model.addToResourceSet(resourceSet, "tml")
				} else {
					return model.addToResourceSet(resourceSet, "tcl")
				}
			}
			AmlModel:
				return model.addToResourceSet(resourceSet, "aml")
			TslModel:
				return model.addToResourceSet(resourceSet, "tsl")
			default:
				throw new RuntimeException('''unknown model='«model.class.name»'.''')
		}
	}

}
