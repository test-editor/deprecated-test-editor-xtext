/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.tcl.dsl.ui.editor

import java.util.List
import java.util.concurrent.atomic.AtomicReference
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.text.source.ISourceViewer
import org.eclipse.jface.text.source.SourceViewer
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.DropTargetAdapter
import org.eclipse.swt.dnd.DropTargetEvent
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.ILocationInFileProvider
import org.eclipse.xtext.resource.XtextResource
import org.testeditor.aml.InteractionType
import org.testeditor.tcl.TclModel
import org.testeditor.aml.Component
import org.eclipse.xtext.naming.IQualifiedNameProvider

class DropTargetXtextEditorListener extends DropTargetAdapter {

	@Inject extension ILocationInFileProvider
	@Inject DropUtils dropUtils
	@Inject TclModelDragAndDropUpdater updateTestModelByDropTarget
	@Inject IQualifiedNameProvider qualifiedNameProvider

	@Inject DropTargetXtextEditor editor

	override void dragEnter(DropTargetEvent event) {
		if ("org.testeditor.tcl.dsl.Tcl" != editor.languageName) {
			event.detail = DND.DROP_NONE
		}
		if (dropUtils.getDroppedObjectAs(InteractionType) === null) {
			event.detail = DND.DROP_NONE
		}
	}

	override void drop(DropTargetEvent event) {
		val List<String> elementPathsToFormat = newArrayList
		val insertedTestStepPath = new AtomicReference<String>
		
		editor.document => [
			modify[withTclModel[it, dropTarget | updateImports(it, dropTarget)]]
			modify[withTclModel[it, dropTarget|updateModel(dropTarget, elementPathsToFormat, insertedTestStepPath)]]
			modify[withTclModel[formatRelevantRegion(elementPathsToFormat)]]
			modify[withTclModel[setCursorAfterInsertedStep(insertedTestStepPath)]]
		]
	}

	private def void updateImports(TclModel tclModel, EObject dropTarget) {
		val droppedObject = dropUtils.getDroppedObjectAs(Component)
		updateTestModelByDropTarget.updateImports(tclModel, dropTarget, droppedObject,
			qualifiedNameProvider.getFullyQualifiedName(droppedObject).toString)
	}

	private def void updateModel(TclModel tclModel, EObject dropTarget, List<String> eObjectPathsToFormat,
		AtomicReference<String> eObjectPath) {
		val droppedTestStepContext = dropUtils.createDroppedTestStepContext
		updateTestModelByDropTarget.updateModel(tclModel, dropTarget, droppedTestStepContext, eObjectPathsToFormat,
			eObjectPath)
	}

	private def void setCursorAfterInsertedStep(TclModel tclModel, AtomicReference<String> eObjectPath) {
		val eObject = EcoreUtil2.getEObject(tclModel, eObjectPath.get)
		val currentRegion = eObject.fullTextRegion
		editor => [
			(internalSourceViewer as SourceViewer).setSelectedRange(currentRegion.offset + currentRegion.length, 0)
			setFocus
		]
	}

	private def void formatRelevantRegion(TclModel tclModel, List<String> eObjectPathsToFormat) {
		val textRegion = eObjectPathsToFormat //
		.map[EcoreUtil2.getEObject(tclModel, it)] //
		.map[fullTextRegion] //
		.reduce[textRegion1, textRegion2|textRegion1.merge(textRegion2)]

		(editor.internalSourceViewer as SourceViewer) => [
			setSelectedRange(textRegion.offset, textRegion.length)
			doOperation(ISourceViewer.FORMAT)
		]
	}

	/**
	 * apply the closure if the resource contents head is of type TclModel, else do nothing
	 */
	private def XtextResource withTclModel(XtextResource resource, (TclModel)=>void consumer) {
		val model = resource.contents.head
		if (model instanceof TclModel) {
			consumer.apply(model)
		}
		return resource
	}

	/**
	 * apply the closure if the resource contents head is of type TclModel using the drop target as 
	 * second parameter (EObject), else do nothing
	 */
	private def XtextResource withTclModel(XtextResource resource, (TclModel, EObject)=>void consumer) {
		val model = resource.contents.head
		if (model instanceof TclModel) {
			consumer.apply(model, editor.findDropTarget(resource))
		}
		return resource
	}

}
