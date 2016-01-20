/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
package org.testeditor.aml.dsl.generator

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.generator.IFileSystemAccess
import org.eclipse.xtext.generator.IGenerator
import org.testeditor.aml.AmlModel

/**
 * Generates the XML for the "all action groups" and "technical bindings" depending
 * on whether {@link AmlModel#getFileNameAllActionGroups()} and
 * {@link AmlModel#getFileNameTechnicalBindings()} are set.
 */
class XmlGenerator implements IGenerator {

	@Inject extension AllActionGroupsGenerator
	@Inject extension TechnicalBindingsGenerator

	override doGenerate(Resource input, IFileSystemAccess fsa) {
		val model = input.contents.head as AmlModel
		if (!model.fileNameAllActionGroups.nullOrEmpty) {
			val fileName = model.packageFolder +  model.fileNameAllActionGroups
			fsa.generateFile(fileName, model.generateAllActionGroups)
		}
		if (!model.fileNameTechnicalBindings.nullOrEmpty) {
			val fileName = model.packageFolder + model.fileNameTechnicalBindings
			fsa.generateFile(fileName, model.generateTechnicalBindings)
		}
	}
	
	protected def String getPackageFolder(AmlModel model) {
		return model.package.replaceAll("\\.", "/") + "/"
	}

}