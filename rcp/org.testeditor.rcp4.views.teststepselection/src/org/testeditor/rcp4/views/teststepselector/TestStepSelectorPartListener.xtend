package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.e4.core.services.events.IEventBroker
import org.eclipse.ui.IPartListener
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.texteditor.ITextEditor

@Creatable
class TestStepSelectorPartListener implements IPartListener {

	@Inject IEventBroker broker

	override partActivated(IWorkbenchPart part) {
		if (part instanceof ITextEditor) {
			broker.post(TestStepSelector.SELECTOR_UPDATE_VIEW, part)
		}
	}

	override partBroughtToTop(IWorkbenchPart part) {
		if (part instanceof ITextEditor) {
			broker.post(TestStepSelector.SELECTOR_UPDATE_VIEW, part)
		}
	}

	override partClosed(IWorkbenchPart part) {
	}

	override partDeactivated(IWorkbenchPart part) {
	}

	override partOpened(IWorkbenchPart part) {
	}

}
