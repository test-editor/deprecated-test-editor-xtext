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
import org.eclipse.xtext.generator.IFileSystemAccess
import org.eclipse.xtext.generator.IGenerator
import org.eclipse.xtext.generator.InMemoryFileSystemAccess
import org.eclipse.xtext.junit4.util.ParseHelper
import org.junit.Before
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.util.TclModelUtil

abstract class AbstractTclGeneratorIntegrationTest extends AbstractTclTest {

	protected ParseHelper<AmlModel> amlParseHelper

	@Inject protected ParseHelper<TclModel> tclParseHelper
	@Inject protected IGenerator generator
	@Inject extension TclModelUtil

	protected InMemoryFileSystemAccess fsa

	@Before
	def void setUp() {
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
		val file = fsa.getJavaFile(model.package, model.name)
		return file.toString
	}
	
	protected def Object getJavaFile(InMemoryFileSystemAccess fsa, String ^package, String name) {
		val key = '''«IFileSystemAccess.DEFAULT_OUTPUT»«package.replaceAll('\\.', '/')»/«name».java'''
		return fsa.allFiles.get(key)
	}

}
