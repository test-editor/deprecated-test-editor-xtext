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
package org.testeditor.rcp4.views.teststepselector

import org.eclipse.emf.ecore.EObject
import org.testeditor.aml.InteractionType
import org.eclipse.e4.core.di.annotations.Creatable
import org.testeditor.aml.Component

/** provide info as to what elements are actually droppable into an tcl editor */
@Creatable
class AmlDropSupport {
	
	// default is false
	def dispatch boolean dropSupported(EObject element){
		false
	}
	
	def dispatch boolean dropSupported(InteractionType element){
		true
	}
	
	def dispatch boolean dropSupported(Component element){
		true
	}
}