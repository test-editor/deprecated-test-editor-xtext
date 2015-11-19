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
package org.testeditor.tcl.dsl.ui.labeling

import com.google.inject.Inject
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.SpecificationStep
import org.testeditor.tcl.TestStep

/**
 * Provides labels for EObjects.
 * 
 * See https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#label-provider
 */
class TclLabelProvider extends org.eclipse.xtext.xbase.ui.labeling.XbaseLabelProvider {

	@Inject
	new(org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider delegate) {
		super(delegate);
	}

	// Labels and icons can be computed like this:
	
	def text(TestStepContext tsc) {
		return "Mask: " + tsc.component.label
	}
	
	def text(SpecificationStep specStep){
		return specStep.contents.map[it.value].join(" ")
	}
	
	def text(TestStep testStep) {
		return testStep.contents.map[it.value].join(" ")		
	}
}
