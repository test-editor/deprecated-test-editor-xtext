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
package org.testeditor.tsl.dsl

import org.testeditor.tsl.TslPackage
import org.eclipse.emf.ecore.EPackage

/**
 * Initialization support for running Xtext languages without Equinox extension registry.
 */
class TslStandaloneSetup extends TslStandaloneSetupGenerated {

	def static void doSetup() {
		new TslStandaloneSetup().createInjectorAndDoEMFRegistration
	}

	override createInjectorAndDoEMFRegistration() {
		EPackage.Registry.INSTANCE.putIfAbsent(TslPackage.eNS_URI, TslPackage.eINSTANCE)
		super.createInjectorAndDoEMFRegistration()
	}

}
