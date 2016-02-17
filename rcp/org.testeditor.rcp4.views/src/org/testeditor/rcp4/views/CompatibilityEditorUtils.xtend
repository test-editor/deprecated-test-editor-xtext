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
package org.testeditor.rcp4.views

import javax.inject.Inject
import org.eclipse.e4.ui.model.application.ui.basic.MPart
import org.eclipse.e4.ui.workbench.modeling.EPartService
import org.eclipse.jface.text.IDocument
import org.eclipse.jface.text.ITextOperationTarget
import org.eclipse.jface.text.ITextViewer
import org.eclipse.ui.IEditorPart
import org.eclipse.ui.internal.e4.compatibility.CompatibilityEditor
import org.eclipse.ui.texteditor.IDocumentProvider
import org.eclipse.ui.texteditor.ITextEditor

class CompatibilityEditorUtils {

	@Inject
	var EPartService partService

	// val String tag ="org.testeditor.dsl.tcl.Tcl"
	def boolean dslEditorWithTagActive(MPart part, String tag) {
		(part != null && part.parent.children.exists [
			(it as MPart).object != null && tags.exists[equals(tag)]
		])
	}

	def IEditorPart activeDSLEditorWithTag(MPart part, String tag) {
		if (dslEditorWithTagActive(part, tag)) {
			return openCompatibilityEditor.editor
		}
		return null
	}

	def void insertTextAtCaret(ITextEditor textEditor, String data) {
		val IDocumentProvider dp = textEditor.documentProvider
		val IDocument doc = dp.getDocument(textEditor.editorInput)
		val cpos = (textEditor.getAdapter(ITextOperationTarget) as ITextViewer).textWidget.caretOffset
		doc.replace(cpos, 0, data);
	}

	def MPart compatibilityEditor() {
		partService.findPart(CompatibilityEditor.MODEL_ELEMENT_ID)
	}

	def CompatibilityEditor openCompatibilityEditor() {
		(compatibilityEditor.parent.children.filter[(it as MPart).object != null].head as MPart).
			object as CompatibilityEditor
	}

	def void passToEditorWithTag(String data, String tag) {
		activeDSLEditorWithTag(compatibilityEditor, tag) => [
			getAdapter(ITextEditor).insertTextAtCaret(data)
			// TODO pass focus to editor (again), does not seem to work, yet
			setFocus
		]
	}

}
