/*******************************************************************************
* Copyright (c) 2012 - 2017 Signal Iduna Corporation and others.
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
package org.testeditor.tcl.dsl.ide

import com.google.inject.Guice
import org.eclipse.xtext.util.Modules2
import org.testeditor.tcl.dsl.TclRuntimeModule
import org.testeditor.tcl.dsl.TclStandaloneSetup

/**
 * Initialization support for running Xtext languages as language servers.
 */
class TclIdeSetup extends TclStandaloneSetup {

	override createInjector() {
		Guice.createInjector(Modules2.mixin(new TclRuntimeModule, new TclIdeModule))
	}
	
}
