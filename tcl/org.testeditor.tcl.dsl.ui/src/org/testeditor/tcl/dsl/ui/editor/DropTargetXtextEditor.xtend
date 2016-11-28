package org.testeditor.tcl.dsl.ui.editor

import javax.inject.Inject
import org.eclipse.jface.text.TextSelection
import org.eclipse.jface.text.source.ISourceViewer
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.ui.dnd.IDragAndDropService
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext

class DropTargetXtextEditor extends XtextEditor {

	@Inject DropTargetXtextEditorListener dropTargetListener
	@Inject protected ContentAssistContext.Factory contentAssistFactory

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

	def findDropTarget(XtextResource resource) {
		val offset = (selectionProvider.selection as TextSelection).offset
		val contentAssistContexts = contentAssistFactory.create(internalSourceViewer, offset, resource)
		return contentAssistContexts.head.currentModel
	}

}
