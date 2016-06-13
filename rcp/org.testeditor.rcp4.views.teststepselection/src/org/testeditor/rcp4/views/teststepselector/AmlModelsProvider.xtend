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
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.IResourceDescriptions
import org.testeditor.aml.AmlModel

import static org.testeditor.aml.AmlPackage.Literals.AML_MODEL

public class AmlModelsProvider {

	@Inject
	IResourceDescriptions resourceDescriptions

	@Inject
	ResourceSet rs

	def Iterable<AmlModel> getModelsForNamespace(String namespace) {
		return amlModels.filter[package == namespace]
	}

	def Iterable<AmlModel> getModelsForNamespaceOf(AmlModel amlModel) {
		return amlModel.package.modelsForNamespace
	}

	def boolean empty() {
		return resourceDescriptions.empty
	}

	def Iterable<AmlModel> getAmlModels() {
		if (empty) {
			return emptySet
		}
		val amlDescriptions = resourceDescriptions.getExportedObjectsByType(AML_MODEL)
		return amlDescriptions.map[EObjectOrProxy].map[EcoreUtil2.resolve(it, rs) as AmlModel]
	}

}
