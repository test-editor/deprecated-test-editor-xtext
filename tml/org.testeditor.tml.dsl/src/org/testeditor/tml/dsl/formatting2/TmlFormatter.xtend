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
package org.testeditor.tml.dsl.formatting2

import com.google.inject.Inject
import org.eclipse.xtext.formatting2.IFormattableDocument
import org.eclipse.xtext.xbase.formatting2.XbaseFormatter
import org.testeditor.tml.TmlModel
import org.testeditor.tml.dsl.services.TmlGrammarAccess

class TmlFormatter extends XbaseFormatter {
	
	@Inject extension TmlGrammarAccess

	def dispatch void format(TmlModel tmlModel, extension IFormattableDocument document) {
		// TODO: format HiddenRegions around keywords, attributes, cross references, etc. 
		tmlModel.getImportSection.format;
	}
	
	// TODO: implement for 
}
