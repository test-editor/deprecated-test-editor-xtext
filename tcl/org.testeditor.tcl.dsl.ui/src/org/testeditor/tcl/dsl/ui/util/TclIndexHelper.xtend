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
package org.testeditor.tcl.dsl.ui.util

import java.util.ArrayList
import java.util.Map
import javax.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.IResourceDescriptions
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase

class TclIndexHelper {
	
	@Inject TclInjectorProvider tclInjectorProvider

	def Map<URI, ArrayList<TestCase>> createTestCaseIndex() {
		var resourceDescriptions = tclInjectorProvider.get.getInstance(IResourceDescriptions)
		val rs = tclInjectorProvider.get.getInstance(ResourceSet)
		val modells = resourceDescriptions.getExportedObjectsByType(TclPackage.Literals.TCL_MODEL)
		val tclModells = modells.map[EObjectOrProxy].map[EcoreUtil2.resolve(it, rs) as TclModel]
		val testcases = tclModells.map[test].filterNull
		val tslIndex = newHashMap
		testcases.forEach [
			if (it.specification != null) {
				val tslKey = it.specification.eContainer.eResource.URI
				if (!tslIndex.keySet.contains(tslKey)) {
					tslIndex.put(tslKey, newArrayList)
				}
				tslIndex.get(tslKey).add(it)
			}
		]
		return tslIndex
	}

}
