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
package org.testeditor.tcl.dsl

import org.eclipse.emf.ecore.EPackage
import org.testeditor.aml.AmlPackage
import org.testeditor.tcl.TclPackage
import org.testeditor.tsl.TslPackage

import static extension org.testeditor.dsl.common.util.CollectionUtils.putIfAbsent
import org.testeditor.tml.TmlPackage

/**
 * Initialization support for running Xtext languages without Equinox extension registry.
 */
class TclStandaloneSetup extends TclStandaloneSetupGenerated {

	def static void doSetup() {
		new TclStandaloneSetup().createInjectorAndDoEMFRegistration
	}

	override createInjectorAndDoEMFRegistration() {
		EPackage.Registry.INSTANCE.putIfAbsent(TslPackage.eNS_URI, TslPackage.eINSTANCE)
		EPackage.Registry.INSTANCE.putIfAbsent(TclPackage.eNS_URI, TclPackage.eINSTANCE)
		EPackage.Registry.INSTANCE.putIfAbsent(AmlPackage.eNS_URI, AmlPackage.eINSTANCE)
		EPackage.Registry.INSTANCE.putIfAbsent(TmlPackage.eNS_URI, TmlPackage.eINSTANCE)
		return super.createInjectorAndDoEMFRegistration
	}

}
