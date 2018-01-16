/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
import java.util.List
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.tcl.dsl.TclRuntimeModule
import org.testeditor.tcl.dsl.TclStandaloneSetup

abstract class AbstractTclTest extends AbstractTest {

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

}
