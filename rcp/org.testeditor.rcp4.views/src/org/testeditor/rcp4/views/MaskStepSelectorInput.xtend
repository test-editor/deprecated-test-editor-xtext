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
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.IResourceDescriptions
import org.testeditor.aml.AmlModel

/** provide the input for the drag and drop tree view of aml model elements */
class MaskStepSelectorInput {

	@Inject
	var IResourceDescriptions resourceDescriptions;

	@Inject
	ResourceSet rs;

	def Iterable<AmlModel> getAmlModels() {
		resourceDescriptions //
		.allResourceDescriptions //
		.map[exportedObjects] //
		.flatten //
		.filter[EObjectOrProxy.eClass.name == AmlModel.simpleName] //
		.map[EObjectOrProxy] //
		.map[EcoreUtil2.resolve(it, rs) as AmlModel]
	}

}
