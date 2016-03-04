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
package org.testeditor.aml.dsl.tests

import com.google.inject.Guice
import com.google.inject.Injector
import com.google.inject.Module
import java.util.ArrayList
import java.util.List
import javax.inject.Inject
import org.eclipse.xtext.junit4.AbstractXtextTests
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.util.Modules2
import org.junit.runner.RunWith
import org.mockito.MockitoAnnotations
import org.testeditor.aml.dsl.AmlRuntimeModule
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.dsl.common.testing.AssertionHelper

@RunWith(XtextRunner)
abstract class AbstractTest extends AbstractXtextTests {
	
	@Inject protected extension AssertionHelper
	@Inject protected extension ValidationTestHelper
	
	override setUp() throws Exception {
		super.setUp()
		
		MockitoAnnotations.initMocks(this)

		// Setup dependency injection
		val injector = createInjector
		setInjector(injector)
		injector.injectMembers(this)
	}
	
	protected def Injector createInjector() {
		val modules = new ArrayList<Module>
		modules += new AmlRuntimeModule
		modules.collectModules
		
		val mixinModule = Modules2.mixin(modules)
		val setup = new AmlStandaloneSetup {
			override createInjector() {
				return Guice.createInjector(mixinModule)
			}
		}
		return setup.createInjectorAndDoEMFRegistration
	}
	
	/**
	 * Subclasses may add modules here, they will be mixed-in.
	 */	
	protected def void collectModules(List<Module> modules) {
	}
	
}