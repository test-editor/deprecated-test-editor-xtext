package org.testeditor.rcp4.views.teststepselector

import org.eclipse.core.commands.IExecutionListener
import org.eclipse.core.commands.NotHandledException
import org.eclipse.core.commands.ExecutionException
import org.eclipse.core.commands.ExecutionEvent
import org.eclipse.ui.IWorkbenchCommandConstants
import org.testeditor.aml.dsl.ui.internal.DslActivator
import org.eclipse.ui.PlatformUI
import javax.inject.Inject
import org.eclipse.e4.core.services.events.IEventBroker
import org.eclipse.e4.core.di.annotations.Creatable

@Creatable
class TestStepSelectorExecutionListener implements IExecutionListener {

	@Inject IEventBroker broker
	
	override notHandled(String commandId, NotHandledException exception) {
	}

	override postExecuteFailure(String commandId, ExecutionException exception) {
	}

	override postExecuteSuccess(String commandId, Object returnValue) {
		if (IWorkbenchCommandConstants.FILE_SAVE.equals(commandId)) {
			val activePage = PlatformUI.workbench.activeWorkbenchWindow.activePage
			val id = activePage.activeEditor.editorSite.id
			if (DslActivator.ORG_TESTEDITOR_AML_DSL_AML.equals(id)) {
				broker.post(TestStepSelector.SELECTOR_TOPIC_UPDATE, null)
			}
		}
	}

	override preExecute(String commandId, ExecutionEvent exception) {
	}
	

}
