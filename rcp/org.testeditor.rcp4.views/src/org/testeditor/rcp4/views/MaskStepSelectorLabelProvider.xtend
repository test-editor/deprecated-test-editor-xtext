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
package org.testeditor.rcp4.views

import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.jface.viewers.LabelProvider

/** e4 label provider for the tree view of aml models */
@Creatable
class MaskStepSelectorLabelProvider extends LabelProvider {
	
	@Inject AmlModelLabelProvider amlModelLabelProvider
	@Inject AmlModelIconProvider amlModelIconProvider
	
	override getImage(Object element) {
		amlModelIconProvider.getIcon(element)
	}
	
	override getText(Object element) {
		amlModelLabelProvider.getText(element)
	}
	
}