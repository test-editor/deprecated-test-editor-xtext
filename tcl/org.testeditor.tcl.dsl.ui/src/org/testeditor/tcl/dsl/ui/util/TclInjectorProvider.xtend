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
package org.testeditor.tcl.dsl.ui.util

import com.google.inject.Injector
import org.eclipse.e4.core.di.annotations.Creatable
import org.testeditor.tcl.dsl.ui.internal.DslActivator

/** provide an injector that is able to inject xtext and thus google guice dependent classes */
@Creatable
class TclInjectorProvider {

	def Injector get() {
		return DslActivator.instance.getInjector(DslActivator.ORG_TESTEDITOR_TCL_DSL_TCL)
	}

}
