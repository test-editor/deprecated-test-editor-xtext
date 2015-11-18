/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
package org.testeditor.tcl.dsl.ui.quickfix

import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.jface.viewers.Viewer
import org.eclipse.jface.viewers.ViewerFilter
import org.eclipse.jface.window.Window
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog
import org.eclipse.ui.model.BaseWorkbenchContentProvider
import org.eclipse.ui.model.WorkbenchLabelProvider
import org.eclipse.ui.part.FileEditorInput
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.ui.editor.quickfix.Fix
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor
import org.eclipse.xtext.validation.Issue
import org.eclipse.xtext.xbase.ui.quickfix.XbaseQuickfixProvider
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.dsl.validation.TclValidator
import org.testeditor.aml.model.AmlModel

/**
 * Custom quickfixes.
 * 
 * See https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#quick-fixes
 */
class TclQuickfixProvider extends XbaseQuickfixProvider {

	@Fix(TclValidator.UNKNOWN_NAME)
	def createAMLMask(Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Create AML Mask", "Creates a new AML Mask", 'upcase.png') [ element, context |
			if (element instanceof TestStepContext) {
				if (element.component.eIsProxy) {
					val sub = NodeModelUtils.findActualNodeFor(element).text.split(':')
					val maskName = sub.get(1).trim
					println(maskName)
					val dialog = new ElementTreeSelectionDialog(Display.getDefault().getActiveShell(),
						new WorkbenchLabelProvider(), new BaseWorkbenchContentProvider())
					dialog.input = ResourcesPlugin.getWorkspace().getRoot()
					dialog.allowMultiple = true
					dialog.title = "Select AML file"
					dialog.addFilter(new ViewerFilter() {

						override select(Viewer viewer, Object parentElement, Object element) {
							if (element instanceof IFile) {
								return element.toString().endsWith("aml")
							} else
								return true
						}

					})
					if (dialog.open == Window.OK) {
						var amlFile = dialog.firstResult as IFile
						var fei = new FileEditorInput(amlFile)
						var id = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(amlFile.getName()).id
						var editor = PlatformUI.workbench.activeWorkbenchWindow.activePage.
							openEditor(fei, id) as XtextEditor

						println(editor.document)
						var lastNode = editor.document.readOnly() [ ressource |
							var amlModel = ressource.contents.head as AmlModel
							return NodeModelUtils.findActualNodeFor(amlModel.components.last)
						]
						editor.document.replace(lastNode.offset + lastNode.length, 0, "\ncomponent " + maskName + " is <TYPE> {\n}")
					}
				}
			}
		]
	}

}

