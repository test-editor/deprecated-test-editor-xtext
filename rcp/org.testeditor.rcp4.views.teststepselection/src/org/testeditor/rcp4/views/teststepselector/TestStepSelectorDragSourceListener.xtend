package org.testeditor.rcp4.views.teststepselector

import org.eclipse.swt.dnd.DragSourceListener
import org.eclipse.swt.dnd.DragSourceEvent
import org.slf4j.LoggerFactory
import org.eclipse.swt.dnd.TextTransfer
import javax.inject.Inject
import org.eclipse.jface.viewers.TreeViewer
import org.eclipse.emf.ecore.EObject
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.jface.util.LocalSelectionTransfer

@Creatable
class TestStepSelectorDragSourceListener implements DragSourceListener {

	@Inject AmlDropSupport amlDropSupport
	@Inject TestStepSelectorDropTextProvider dropTextProvider
	static val logger = LoggerFactory.getLogger(TestStepSelectorDragSourceListener);
	TreeViewer viewer

	public def void setViewer(TreeViewer viewer) {
		this.viewer = viewer
	}

	override dragFinished(DragSourceEvent event) {
		logger.info("drag stop")
	}

	override dragSetData(DragSourceEvent event) {
		LocalSelectionTransfer.transfer.selection = viewer.structuredSelection
		logger.info("set selection " + viewer.structuredSelection)
		event.data = "test"
		if (false) {
			if (TextTransfer.instance.isSupportedType(event.dataType) &&
				amlDropSupport.dropSupported(viewer.structuredSelection.firstElement as EObject)) {
				event.data = dropTextProvider.getText(viewer.structuredSelection)
				logger.info("droped text: " + event.data)
			} else {
				event.data = ""
			}
		}
	}

	override dragStart(DragSourceEvent event) {
		LocalSelectionTransfer.transfer.selection = viewer.structuredSelection
		logger.info("set selection " + viewer.structuredSelection)
		logger.info("drag start oho")
	}

}
