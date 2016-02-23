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

import java.util.Collections
import java.util.List
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.ModelUtil

/** adapt the aml model to the tree actually displayed in the drag and drop view for insertion in a tcl document */
public class AmlModelTreeAdapter {

	@Inject
	ModelUtil modelUtil

	@Inject
	AmlModelsProvider amlModelsProvider

	def dispatch List<EObject> children(AmlModel node) {
		node.components.map[it as EObject]
	}

	def dispatch List<EObject> children(Component node) {
		// get all components of the same namespace
		val models = amlModelsProvider.getModelsForNamespaceOf(node.eContainer as AmlModel);
		val componentsOfNamespace = (models.map[components].flatten + #[node]).toSet // node itself is added just in case the models cannot be retrieved
		return componentsOfNamespace.map [
			(modelUtil.getAllComponentElements(it).map[it as EObject] + modelUtil.getAllComponentInteractionTypes(it))
		].flatten.toList
	}

	def dispatch List<EObject> children(ComponentElement node) {
		modelUtil.getComponentElementInteractionTypes(node).map[it as EObject].toList
	}

	def dispatch List<EObject> children(EObject node) {
		return Collections.emptyList
	}

	def EObject parent(EObject node) {
		node.eContainer
	}

}
