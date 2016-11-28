package org.testeditor.tcl.dsl.ui.editor

import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.jface.text.source.ISourceViewer
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.ui.dnd.IDragAndDropService
import javax.inject.Inject

class DropTargetXtextEditor extends XtextEditor {

	@Inject DropTargetXtextEditorListener dropTargetListener

	override protected installTextDragAndDrop(ISourceViewer viewer) {
		if (viewer === null)
			return

		val dndService = getSite().getService(IDragAndDropService)
		if (dndService === null)
			return;

		// Install drag target
		dropTargetListener.editor = this
		dndService.addMergedDropTarget(viewer.getTextWidget(), DND.DROP_MOVE.bitwiseOr(DND.DROP_COPY),
			#[TextTransfer.instance], dropTargetListener);
	}
}
