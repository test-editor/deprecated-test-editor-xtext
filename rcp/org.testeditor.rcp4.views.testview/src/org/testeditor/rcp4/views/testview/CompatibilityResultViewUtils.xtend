package org.testeditor.rcp4.views.testview

import javax.inject.Inject
import org.eclipse.core.runtime.FileLocator
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.Path
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.e4.ui.model.application.ui.basic.MPart
import org.eclipse.e4.ui.workbench.modeling.EPartService
import org.eclipse.jdt.internal.junit.ui.TestRunnerViewPart
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.ui.internal.e4.compatibility.CompatibilityView
import org.osgi.framework.FrameworkUtil

@Creatable
class CompatibilityResultViewUtils {
	@Inject EPartService partService

	val bundle = FrameworkUtil.getBundle(CompatibilityResultViewUtils)

	def void importRunSession(String resource) {
		val resourceUrl = FileLocator.find(bundle, new Path(resource), null)
		JUnitCore.importTestRunSession(resourceUrl.toString, new NullProgressMonitor)
	}

	def TestRunnerViewPart getTestRunnerViewPart() {
		val object = resultView.object
		return (object as CompatibilityView ).part as TestRunnerViewPart
	}

	def MPart getResultView() {
		partService.findPart(TestRunnerViewPart.NAME)
	}

}
