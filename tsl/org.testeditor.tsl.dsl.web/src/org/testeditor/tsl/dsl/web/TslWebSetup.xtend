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
package org.testeditor.tsl.dsl.web

import com.google.inject.Guice
import com.google.inject.Injector
import org.eclipse.xtext.util.Modules2
import org.testeditor.tsl.dsl.TslRuntimeModule
import org.testeditor.tsl.dsl.TslStandaloneSetup
import org.testeditor.tsl.dsl.ide.TslIdeModule

/**
 * Initialization support for running Xtext languages in web applications.
 */
class TslWebSetup extends TslStandaloneSetup {
	
	override Injector createInjector() {
		return Guice.createInjector(Modules2.mixin(new TslRuntimeModule, new TslIdeModule, new TslWebModule))
	}
	
}
