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
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.viewers.ITreeContentProvider
import org.eclipse.jface.viewers.Viewer
import org.testeditor.aml.AmlModel

/** e4 tree content provider for the tree view of aml models */
class MaskStepSelectorTreeContentProvider implements ITreeContentProvider {

	@Inject
	var AmlModelTreeAdapter amlModelTreeAdapter

	@Inject
	var AmlModelLabelProvider amlModelLabelProvider

	override getChildren(Object parentElement) {
		(amlModelTreeAdapter.children(parentElement as EObject)).toArray
	}

	override getElements(Object inputElement) {
		val clonedList = (inputElement as Iterable<AmlModel>).clone // may not return the input itself (see BUG in overridden method)
		// merge duplicate name spaces
		val knownNamespace = clonedList.map[amlModelLabelProvider.getText(it)].toSet
		return clonedList.map [
			if (knownNamespace.contains(amlModelLabelProvider.getText(it))) {
				return null
			} else {
				return it
			}
		].filterNull
	}

	override getParent(Object element) {
		amlModelTreeAdapter.parent(element as EObject)
	}

	override hasChildren(Object element) {
		!getChildren(element).empty
	}

	override dispose() {
	}

	override inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}

}
