package org.testeditor.tcl.dsl.ui.util

import java.util.ArrayList
import java.util.Map
import javax.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.IResourceDescriptions
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase

class TclIndexHelper {
	
	@Inject TclInjectorProvider tclInjectorProvider
	
	def Map<URI, ArrayList<TestCase>> createTestCaseIndex() {
		var resourceDescriptions = tclInjectorProvider.get.getInstance(IResourceDescriptions)
		val rs = tclInjectorProvider.get.getInstance(ResourceSet)
		val testCaseDescriptions = resourceDescriptions.getExportedObjectsByType(TclPackage.Literals.TEST_CASE)
		// resolve of eobjectOrProxy kicks off generation (which fails with this resource set)
		// doing this just to get the specifications is quite heavy weight
		// TODO: find some lightweight alternative
		val testcases = testCaseDescriptions.map[EcoreUtil2.resolve(EObjectOrProxy,rs)].filter(TestCase)
		val tslIndex = newHashMap
		testcases.forEach [
			if (it.specification != null) {
				val tslKey = it.specification.eContainer.eResource.URI
				if (!tslIndex.containsKey(tslKey)) {
					tslIndex.put(tslKey, newArrayList)
				}
				tslIndex.get(tslKey).add(it)
			}
		]
		return tslIndex
	}

}
