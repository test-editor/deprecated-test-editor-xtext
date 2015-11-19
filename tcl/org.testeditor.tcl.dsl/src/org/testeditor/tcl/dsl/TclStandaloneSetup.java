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
package org.testeditor.tcl.dsl;

import org.eclipse.emf.ecore.EPackage;
import org.testeditor.tcl.TclPackage;
import org.testeditor.tsl.TslPackage;

import com.google.inject.Injector;

/**
 * Initialization support for running Xtext languages 
 * without equinox extension registry
 */
public class TclStandaloneSetup extends TclStandaloneSetupGenerated{

	public static void doSetup() {
		new TclStandaloneSetup().createInjectorAndDoEMFRegistration();
	}
	
	@Override
	public Injector createInjectorAndDoEMFRegistration() {
	    EPackage.Registry.INSTANCE.put(TslPackage.eINSTANCE.getNsURI(), TslPackage.eINSTANCE);
	    EPackage.Registry.INSTANCE.put(TclPackage.eINSTANCE.getNsURI(), TclPackage.eINSTANCE);
	    return super.createInjectorAndDoEMFRegistration();
	}
}

