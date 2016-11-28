package org.testeditor.tcl.dsl.ui.editor

import java.util.List
import org.eclipse.swt.dnd.DropTargetAdapter
import org.eclipse.swt.dnd.DropTargetEvent
import org.eclipse.xtext.resource.XtextResource
import org.testeditor.aml.InteractionType
import org.testeditor.tcl.TclModel
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.jface.text.source.SourceViewer
import org.eclipse.jface.text.source.ISourceViewer
import javax.inject.Inject
import org.eclipse.swt.dnd.DND
import org.eclipse.xtext.resource.ILocationInFileProvider
import org.eclipse.xtend.lib.annotations.Accessors

class DropTargetXtextEditorListener extends DropTargetAdapter {

	@Inject extension ILocationInFileProvider
	@Inject private DropUtils dropUtils
	@Inject private UpdateTestModelByDropTarget updateTestModelByDropTarget

	@Accessors(PUBLIC_SETTER)
	private DropTargetXtextEditor editor

	override void dragEnter(DropTargetEvent event) {
		if ("org.testeditor.tcl.dsl.Tcl" != editor.languageName) {
			event.detail = DND.DROP_NONE
		}
		if (dropUtils.getDroppedObjectAs(InteractionType) == null) {
			event.detail = DND.DROP_NONE
		}
	}

	override void drop(DropTargetEvent event) {

		val List<String> toFormat = newArrayList
		val List<String> currentElement = newArrayList

		editor.document.modify[updateModel(toFormat, currentElement)]
		editor.document.modify [
			formatRelevantRegion(toFormat)
		]
		editor.document.modify [
			setCursorToNewElement(currentElement)
		]

	}

	private def updateModel(XtextResource resource, List<String> toFormat, List<String> currentElement) {
		updateTestModelByDropTarget.updateModel(resource, editor, toFormat, currentElement)

	}

	private def setCursorToNewElement(XtextResource resource, List<String> currentElement) {
		val tclModel = resource.contents.head
		if (tclModel instanceof TclModel) {
			val eObject = EcoreUtil2.getEObject(tclModel, currentElement.head)
			val currentRegion = eObject.fullTextRegion;

			(editor.internalSourceViewer as SourceViewer).setSelectedRange(currentRegion.offset +
				currentRegion.length, 0)
			editor.setFocus

		}
		return editor
	}

	private def formatRelevantRegion(XtextResource resource, List<String> toFormat) {

		val tclModel = resource.contents.head
		if (tclModel instanceof TclModel) {

			val textRegion = toFormat //
			.map[EcoreUtil2.getEObject(tclModel, it)] //
			.map[fullTextRegion] //
			.reduce[textRegion1, textRegion2|textRegion1.merge(textRegion2)]

			(editor.internalSourceViewer as SourceViewer) => [
				setSelectedRange(textRegion.offset, textRegion.length)
				doOperation(ISourceViewer.FORMAT)
			]
		}
	}

}
