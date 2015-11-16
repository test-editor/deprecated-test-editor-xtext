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
package org.testeditor.tcl.dsl.formatting2;

import com.google.inject.Inject
import org.eclipse.xtext.formatting2.IFormattableDocument
import org.eclipse.xtext.xbase.formatting2.XbaseFormatter
import org.testeditor.tcl.ComponentReference
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.services.TclGrammarAccess

class TclFormatter extends XbaseFormatter {
	
	@Inject extension TclGrammarAccess

	def dispatch void format(TclModel tclmodel, extension IFormattableDocument document) {
		// TODO: format HiddenRegions around keywords, attributes, cross references, etc. 
		format(tclmodel.getImportSection(), document);
		for (ComponentReference elements : tclmodel.getElements()) {
			format(elements, document);
		}
	}
}
