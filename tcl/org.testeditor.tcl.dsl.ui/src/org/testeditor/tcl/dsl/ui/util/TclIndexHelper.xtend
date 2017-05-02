package org.testeditor.tcl.dsl.ui.util

import java.util.List
import java.util.Map
import javax.inject.Inject
import org.eclipse.core.resources.IProject
import org.eclipse.emf.common.util.URI
import org.eclipse.jdt.core.JavaCore
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.IResourceDescriptions
import org.eclipse.xtext.resource.XtextResourceSet
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase

class TclIndexHelper {
	
	@Inject TclInjectorProvider tclInjectorProvider

	def Map<URI, List<TestCase>> createTestCaseIndex(IProject project) {
 		var resourceDescriptions = tclInjectorProvider.get.getInstance(IResourceDescriptions)
		val rs = tclInjectorProvider.get.getInstance(XtextResourceSet)
		rs.classpathURIContext = JavaCore.create(project) // make sure org.junit.Test is on the resolvable classpath (is used by generator, which again is used when resolving the test cases)
		val testCaseDescriptions = resourceDescriptions.getExportedObjectsByType(TclPackage.Literals.TEST_CASE)
		val testcases = testCaseDescriptions.map[EcoreUtil2.resolve(EObjectOrProxy,rs)].filter(TestCase)
		val tslIndex = newHashMap
		testcases.forEach [
			if (it.specification != null) {
				val tslKey = it.specification.eContainer.eResource.URI
				if (!tslIndex.containsKey(tslKey)) {
					val List<TestCase> list = newArrayList
					tslIndex.put(tslKey, list)
				}
				tslIndex.get(tslKey).add(it)
			}
		]
		return tslIndex
	}

}
