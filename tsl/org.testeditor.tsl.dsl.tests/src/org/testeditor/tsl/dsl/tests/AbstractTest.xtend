/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
package org.testeditor.tsl.dsl.tests

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
import org.testeditor.dsl.common.testing.AssertionHelper
import org.testeditor.tsl.dsl.TslRuntimeModule
import org.testeditor.tsl.dsl.TslStandaloneSetup
import org.eclipse.emf.ecore.EPackage
import org.testeditor.tsl.TslPackage

@RunWith(XtextRunner)
abstract class AbstractTest extends AbstractXtextTests {
	
	@Inject protected extension AssertionHelper
	@Inject protected extension ValidationTestHelper
	
	override setUp() throws Exception {
		super.setUp()
		
		// Setup dependency injection
		val injector = createInjector
		setInjector(injector)
		injector.injectMembers(this)
		
		if( !EPackage.Registry.INSTANCE.containsKey("http://www.testeditor.org/tsl")){
			EPackage.Registry.INSTANCE.put("http://www.testeditor.org/tsl", TslPackage.eINSTANCE)
		}
		
		
	}
	
	protected def Injector createInjector() {
		val modules = new ArrayList<Module>
		modules += new TslRuntimeModule
		modules.collectModules
		
		val mixinModule = Modules2.mixin(modules)
		val setup = new TslStandaloneSetup {
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