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

import java.util.List
import java.util.Map
import javax.inject.Inject
import org.eclipse.jface.viewers.ITreeContentProvider
import org.eclipse.jface.viewers.Viewer
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.ModelUtil

/** e4 tree content provider for the tree view of aml models */
class TestStepSelectorTreeContentProvider implements ITreeContentProvider {

	@Inject ModelUtil modelUtil

	Map<String, List<AmlModel>> input

	override getChildren(Object parentElement) {
		return switch (parentElement) {
			String: {
				// A String means we have a namespace / package
				val models = input.get(parentElement)
				models.map[components].flatten
			}
			Component: {
				// Interaction Types for the Component itself + its elements
				modelUtil.getComponentInteractionTypes(parentElement) + modelUtil.getComponentElements(parentElement)
			}
			ComponentElement: {
				modelUtil.getComponentElementInteractionTypes(parentElement)
			}
			default:
				#[]
		}
	}

	override getElements(Object inputElement) {
		return input.keySet
	}

	override getParent(Object element) {
		return null
	}

	override hasChildren(Object element) {
		return !element.children.empty
	}

	override dispose() {
	}

	override inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		val input = newInput as Iterable<AmlModel>
		this.input = input?.groupBy[package]
	}

}
