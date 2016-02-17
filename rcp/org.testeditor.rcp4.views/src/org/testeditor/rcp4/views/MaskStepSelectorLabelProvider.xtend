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

import org.eclipse.jface.viewers.ILabelProvider
import org.eclipse.jface.viewers.ILabelProviderListener
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.e4.core.di.annotations.Creatable

/** e4 label provider for the tree view of aml models */
@Creatable
class MaskStepSelectorLabelProvider implements ILabelProvider{
	
	@Inject
	AmlModelLabelProvider amlModelLabelProvider
	
	@Inject
	AmlModelIconProvider amlModelIconProvider
	
	override getImage(Object element) {
		amlModelIconProvider.getIcon(element as EObject)
	}
	
	override getText(Object element) {
		amlModelLabelProvider.getText(element as EObject)
	}
	
	override addListener(ILabelProviderListener listener) {

	}
	
	override dispose() {

	}
	
	override isLabelProperty(Object element, String property) {
		false
	}
	
	override removeListener(ILabelProviderListener listener) {

	}
	
}