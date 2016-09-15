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
package org.testeditor.tcl.dsl.jvmmodel

import org.testeditor.tsl.StepContent
import org.testeditor.tcl.VariableReference

/** provide an interface for resolving variables 
 * (e.g. in calls to macros the variable of the call location must be used, not the parameter (name) of the macro definition itself)
 */
interface VariableResolver {
	
	def StepContent resolveVariableReference(VariableReference referencedVariable)	
	
}