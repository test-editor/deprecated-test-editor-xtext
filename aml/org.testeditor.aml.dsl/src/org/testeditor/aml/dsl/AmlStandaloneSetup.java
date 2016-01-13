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
package org.testeditor.aml.dsl;

import org.eclipse.emf.ecore.EPackage;
import org.testeditor.aml.AmlPackage;

import com.google.inject.Injector;

public class AmlStandaloneSetup extends AmlStandaloneSetupGenerated {

	public static void doSetup() {
		new AmlStandaloneSetup().createInjectorAndDoEMFRegistration();
	}

	@Override
	public Injector createInjectorAndDoEMFRegistration() {
		EPackage.Registry.INSTANCE.put(AmlPackage.eNS_URI, AmlPackage.eINSTANCE);
		return super.createInjectorAndDoEMFRegistration();
	}

}
