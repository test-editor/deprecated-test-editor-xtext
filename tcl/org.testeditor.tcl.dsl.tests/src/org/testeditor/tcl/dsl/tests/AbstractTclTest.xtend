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
package org.testeditor.tcl.dsl.tests

import com.google.inject.Module
import com.google.inject.Provider
import java.util.List
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.resource.XtextResourceSet
import org.junit.Before
import org.testeditor.aml.AmlModel
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.TclRuntimeModule
import org.testeditor.tcl.dsl.TclStandaloneSetup
import org.testeditor.tsl.TslModel

abstract class AbstractTclTest extends AbstractTest {

	@Inject protected Provider<XtextResourceSet> resourceSetProvider
	@Inject protected XtextResourceSet resourceSet


	@Before
	def void setUpResourceSet() {
		resourceSet = resourceSetProvider.get
		resourceSet.classpathURIContext = this		
	}
		
	override protected createInjector() {
		val injector = super.createInjector()
		val standaloneSetup = new TclStandaloneSetup {
			override createInjector() {
				return injector
			}
		}
		standaloneSetup.createInjectorAndDoEMFRegistration
		return injector
	}

	override protected collectModules(List<Module> modules) {
		super.collectModules(modules)
		modules += new TclRuntimeModule
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
