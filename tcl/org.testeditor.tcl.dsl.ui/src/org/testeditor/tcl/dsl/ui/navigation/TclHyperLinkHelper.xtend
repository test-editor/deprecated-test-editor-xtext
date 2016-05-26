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
package org.testeditor.tcl.dsl.ui.navigation

import javax.inject.Inject
import org.eclipse.xtext.ui.editor.hyperlinking.IHyperlinkAcceptor
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tml.dsl.ui.navigation.TmlHyperLinkHelper
import org.testeditor.tsl.SpecificationStep

import static org.testeditor.tsl.TslPackage.Literals.*

class TclHyperLinkHelper extends TmlHyperLinkHelper {

	@Inject extension TclModelUtil

	/**
	 * Create a hyperlink from a {@link SpecificationStepImplementation} to the {@link SpecificationStep}.
	 */
	protected def dispatch void createHyperlinks(SpecificationStepImplementation stepImpl,
		IHyperlinkAcceptor acceptor) {
		val specificationStep = stepImpl.specificationStep
		if (specificationStep !== null) {
			stepImpl.createHyperlinkTo(SPECIFICATION_STEP__CONTENTS, specificationStep, acceptor)
		}
	}

}
