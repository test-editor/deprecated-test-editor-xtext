package org.testeditor.rcp4.views.teststepselector

import org.eclipse.swt.dnd.DragSourceAdapter
import org.eclipse.swt.dnd.DragSourceEvent
import org.slf4j.LoggerFactory
import org.eclipse.jface.viewers.TreeViewer
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.jface.util.LocalSelectionTransfer

@Creatable
class TestStepSelectorDragSourceListener extends DragSourceAdapter {

	TreeViewer viewer

	public def void setViewer(TreeViewer viewer) {
		this.viewer = viewer
	}

	override dragSetData(DragSourceEvent event) {
		// The data-element has to be set to make drag and drop work but the 
		// dragged information is transfered by the LocalSelectionTransfer class
		event.data = "notUsed"
	}

	override dragStart(DragSourceEvent event) {
		LocalSelectionTransfer.transfer.selection = viewer.structuredSelection
	}

}
