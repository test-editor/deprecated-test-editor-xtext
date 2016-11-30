package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.eclipse.core.commands.ExecutionEvent
import org.eclipse.core.commands.ExecutionException
import org.eclipse.core.commands.IExecutionListener
import org.eclipse.core.commands.NotHandledException
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.e4.core.services.events.IEventBroker
import org.eclipse.ui.IWorkbenchCommandConstants
import org.eclipse.ui.PlatformUI
import org.testeditor.aml.dsl.ui.internal.DslActivator

@Creatable
class TestStepSelectorExecutionListener implements IExecutionListener {

	@Inject IEventBroker broker

	override notHandled(String commandId, NotHandledException exception) {
	}

	override postExecuteFailure(String commandId, ExecutionException exception) {
	}

	override postExecuteSuccess(String commandId, Object returnValue) {
		if (commandId == IWorkbenchCommandConstants.FILE_SAVE) {
			val activePage = PlatformUI.workbench.activeWorkbenchWindow.activePage
			val id = activePage.activeEditor.editorSite.id
			if (id == DslActivator.ORG_TESTEDITOR_AML_DSL_AML) {
				broker.post(TestStepSelector.SELECTOR_UPDATE_VIEW, activePage.getActiveEditor())
			}
		}
	}

	override preExecute(String commandId, ExecutionEvent exception) {
	}

}
