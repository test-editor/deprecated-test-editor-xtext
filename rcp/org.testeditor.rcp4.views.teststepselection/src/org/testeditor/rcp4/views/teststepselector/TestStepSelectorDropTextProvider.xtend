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
package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.jface.viewers.ITreeSelection

/** provide the text that should be dropped into the tcl document, given the tree selection */
@Creatable
class TestStepSelectorDropTextProvider {

	@Inject
	var TestStepSelectorLabelProvider labelProvider

	@Inject
	var AmlModelTreeDropTextProvider amlDropTextProvider

	/** get the base text via label provider and then give the drop text provider the
	 *  chance to modify this base text walking through the complete tree path, starting
	 *  at the selected element itself up to the root
	 */
	def String getText(ITreeSelection selection) {
		val firstElement = selection.firstElement
		var result = labelProvider.getText(firstElement)
		val size = selection.paths.head.segmentCount
		if (size > 1) {
			for (var i = size - 1; i >= 0; i--) { // start the dropped element itself, going up the tree
				val element = selection.paths.head.getSegment(i)
				result = amlDropTextProvider.adjustTextBy(size - i - 1, element, result)
			}
		}
		return result
	}

}
