/*******************************************************************************
 * Copyright (c) 2012 - 2017 Signal Iduna Corporation and others.
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
package org.testeditor.tcl.dsl.serializer

import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynNavigable

class TclSyntacticSequencer extends AbstractTclSyntacticSequencer {

	/**
	 * This customization is required for drag&drop functionality in the UI.
	 * It might happen that we need to create new component context elements in
	 * memory which then get serialized. The grammar is ambiguous here as we can
	 * either use the keyword "Mask" or "Component". We emit "Mask" right now, but
	 * this might change in the future.
	 */
	override protected emit_ComponentTestStepContext_ComponentKeyword_1_1_or_MaskKeyword_1_0(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		if (nodes === null) {
			// nodes is null that means the element has been created in memory => we want the "Mask" keyword for now
			val keyword = grammarAccess.componentTestStepContextAccess.maskKeyword_1_0
			acceptUnassignedKeyword(keyword, keyword.value, null)
		} else {
			super.emit_ComponentTestStepContext_ComponentKeyword_1_1_or_MaskKeyword_1_0(semanticObject, transition, nodes)
		}
	}

}
