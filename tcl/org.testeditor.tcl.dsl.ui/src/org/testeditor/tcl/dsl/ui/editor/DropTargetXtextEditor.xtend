package org.testeditor.tcl.dsl.ui.editor

import org.eclipse.xtext.ui.editor.XtextEditor 
import org.eclipse.jface.text.source.ISourceViewer
import org.eclipse.swt.custom.StyledText
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.swt.dnd.DropTargetListener
import org.eclipse.ui.dnd.IDragAndDropService
import javax.inject.Inject

class DropTargetXtextEditor extends XtextEditor {

	var ISourceViewer viewer
	@Inject DropTargetXtextEditorListener dropTargetListener

	override protected installTextDragAndDrop(ISourceViewer viewer) {
		super.installTextDragAndDrop(viewer)
		this.viewer = viewer

		val fIsTextDragAndDropInstalled = false

		if (viewer == null || fIsTextDragAndDropInstalled)
			return

		val dndService = getSite().getService(IDragAndDropService)
		if (dndService == null)
			return

		val StyledText st = viewer.getTextWidget()

		// Install drag target
		dropTargetListener.editor = this
		val DropTargetListener dropTargetListener = dropTargetListener
		
		dndService.addMergedDropTarget(st, DND.DROP_MOVE.bitwiseOr(DND.DROP_COPY), #[TextTransfer.instance],
			dropTargetListener);
	}
	public def ISourceViewer getSourceViewerPublic() {
		super.getSourceViewer
	}
}
