package org.testeditor.rcp4.views.testview

import com.google.common.io.Resources
import javax.inject.Inject
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.e4.ui.model.application.ui.basic.MPart
import org.eclipse.e4.ui.workbench.modeling.EPartService
import org.eclipse.jdt.internal.junit.ui.TestRunnerViewPart
import org.eclipse.jdt.junit.JUnitCore
import org.eclipse.ui.internal.e4.compatibility.CompatibilityView

@Creatable
class CompatibilityResultViewUtils {
	@Inject EPartService partService

	def void importRunSession(String resource) {
		val resourceUrl = Resources.getResource(resource)
		JUnitCore.importTestRunSession(resourceUrl.toString, new NullProgressMonitor)
	}

	def TestRunnerViewPart getTestRunnerViewPart() {
		val cview = resultView.object as CompatibilityView
		return cview.part as TestRunnerViewPart
	}

	def MPart getResultView() {
		partService.findPart(TestRunnerViewPart.NAME)
	}

}
