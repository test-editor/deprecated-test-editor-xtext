package org.testeditor.tcl.dsl.ui.editor

import java.util.List
import java.util.concurrent.atomic.AtomicReference
import javax.inject.Inject
import org.eclipse.jface.text.source.ISourceViewer
import org.eclipse.jface.text.source.SourceViewer
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.DropTargetAdapter
import org.eclipse.swt.dnd.DropTargetEvent
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.ILocationInFileProvider
import org.eclipse.xtext.resource.XtextResource
import org.testeditor.aml.InteractionType
import org.testeditor.tcl.TclModel

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

		val List<String> elementPathsToFormat = newArrayList
		val insertedTestStepPath = new AtomicReference<String>

		editor.document => [
			modify[updateModel(elementPathsToFormat, insertedTestStepPath)]
			modify[formatRelevantRegion(elementPathsToFormat)]
			modify[setCursorAfterInsertedStep(insertedTestStepPath)]
		]
	}

	private def XtextResource updateModel(XtextResource resource, List<String> toFormat,
		AtomicReference<String> eObjectPath) {
		val dropTarget = editor.findDropTarget(resource)
		val model = resource.contents.head
		if (model instanceof TclModel) {
			val droppedTestStepContext = dropUtils.createDroppedTestStepContext
			updateTestModelByDropTarget.updateModel(model, dropTarget, droppedTestStepContext, toFormat, eObjectPath)
		}
		return resource
	}

	private def XtextResource setCursorAfterInsertedStep(XtextResource resource, AtomicReference<String> eObjectPath) {
		val tclModel = resource.contents.head
		if (tclModel instanceof TclModel) {
			val eObject = EcoreUtil2.getEObject(tclModel, eObjectPath.get)
			val currentRegion = eObject.fullTextRegion
			editor => [
				(internalSourceViewer as SourceViewer). //
				setSelectedRange(currentRegion.offset + currentRegion.length, 0)
				setFocus
			]
		}
		return resource
	}

	private def XtextResource formatRelevantRegion(XtextResource resource, List<String> eObjectPathsToFormat) {

		val tclModel = resource.contents.head
		if (tclModel instanceof TclModel) {

			val textRegion = eObjectPathsToFormat //
			.map[EcoreUtil2.getEObject(tclModel, it)] //
			.map[fullTextRegion] //
			.reduce[textRegion1, textRegion2|textRegion1.merge(textRegion2)]

			(editor.internalSourceViewer as SourceViewer) => [
				setSelectedRange(textRegion.offset, textRegion.length)
				doOperation(ISourceViewer.FORMAT)
			]
		}

		return resource
	}

}
