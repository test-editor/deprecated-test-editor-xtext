package org.testeditor.tcl.dsl.ui.util

import java.util.ArrayList
import java.util.HashMap
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

	def HashMap<URI, ArrayList<TestCase>> createTestCaseIndex() {
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
