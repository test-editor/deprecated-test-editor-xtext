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
package org.testeditor.rcp4.views.projectexplorer

import javax.inject.Inject
import org.eclipse.core.resources.IFile
import org.eclipse.jface.action.Action
import org.eclipse.jface.action.IAction
import org.eclipse.jface.viewers.ISelectionProvider
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.ui.IWorkbenchPage
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.part.FileEditorInput
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.utils.EditorUtils
import org.eclipse.xtext.ui.refactoring.ui.IRenameContextFactory
import org.eclipse.xtext.ui.refactoring.ui.IRenameElementContext
import org.eclipse.xtext.ui.refactoring.ui.IRenameSupport
import org.eclipse.xtext.util.concurrent.IUnitOfWork
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase

class RenameAction extends Action {

	IAction resourceReanemAction
	ISelectionProvider selectionProvider
	IWorkbenchPage workbenchPage
	@Inject
	NewNameReciver newNameReciver
	@Inject
	IRenameSupport.Factory renameSupportFactory;

	@Inject
	IRenameContextFactory renameContextFactory;

	new(IAction fileRenameAction, ISelectionProvider provider, IWorkbenchPage page) {
		super("Rename...")
		resourceReanemAction = fileRenameAction
		selectionProvider = provider
		workbenchPage = page
		newNameReciver = new NewNameReciver()
	}

	override run() {
		val selection = selectionProvider.selection
		if (selection instanceof IStructuredSelection) {
			val selectedElement = selection.firstElement
			if (selectedElement instanceof IFile) {
				if (selectedElement.fileExtension.equals("tcl")) {
					renameTestCase(selectedElement)
				} else {
					resourceReanemAction.run
				}
			} else {
				resourceReanemAction.run
			}
		}
	}

	def renameTestCase(IFile testCaseFile) {
		val desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(testCaseFile.getName());
		workbenchPage.openEditor(new FileEditorInput(testCaseFile), desc.id)
		val editor = EditorUtils.getActiveXtextEditor();
		val test = editor.document.readOnly(new IUnitOfWork<TestCase, XtextResource>() {

			override exec(XtextResource state) throws Exception {
				val tcl = state.contents.filter(TclModel).head
				return tcl.test
			}

		})
		val newName = newNameReciver.getNewName(test.name)
		if (newName != null) {
			val textContent = editor.document.get
			val nameSymIndex = textContent.indexOf("#")
			editor.selectAndReveal(textContent.indexOf(test.name, nameSymIndex), test.name.length)

			val renameElementContext = editor.getDocument().readOnly(
				new IUnitOfWork<IRenameElementContext, XtextResource>() {
					override def IRenameElementContext exec(XtextResource state) {
						renameContextFactory.createRenameElementContext(test, editor, null, state);
					}
				});

			val rename = renameSupportFactory.create(renameElementContext, newName);
			rename.startDirectRefactoring();
		}
	}

}
