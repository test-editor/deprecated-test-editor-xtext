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
package org.testeditor.tcl.dsl.ui.quickfix

import javax.inject.Inject
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
import org.eclipse.xtext.nodemodel.ICompositeNode
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.ui.editor.quickfix.Fix
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor
import org.eclipse.xtext.validation.Issue
import org.eclipse.xtext.xbase.ui.quickfix.XbaseQuickfixProvider
import org.testeditor.aml.AmlModel
import org.testeditor.tcl.TestCase
import org.testeditor.tml.TestStepContext
import org.testeditor.tcl.dsl.messages.TclSyntaxErrorMessageProvider
import org.testeditor.tcl.dsl.validation.TclValidator
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tml.ComponentTestStepContext

/**
 * Custom quickfixes.
 * 
 * See https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#quick-fixes
 */
class TclQuickfixProvider extends XbaseQuickfixProvider {

	@Inject extension TclModelUtil

	@Fix(TclValidator.NO_VALID_IMPLEMENTATION)
	def addMissingTestSpecificationSteps(Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Create missing test steps", "Creates missing test steps from the specification.",
			'upcase.png') [ element, context |
			if (element instanceof TestCase) {
				var ICompositeNode lastNode = null
				if (element.steps.empty) {
					lastNode = NodeModelUtils.findActualNodeFor(element)
				} else {
					lastNode = NodeModelUtils.findActualNodeFor(element.steps.last)
				}
				val doc = context.xtextDocument
				val updateNode = lastNode
				val steps = getMissingTestSteps(element)
				doc.replace(updateNode.offset + updateNode.length, 0, getStepsDSLFragment(steps))
			}
		]
	}

	def String getStepsDSLFragment(Iterable<SpecificationStep> steps) '''
		«FOR step : steps»
		
		
		* «step.contents.restoreString»
		
		«ENDFOR»
	'''

	@Fix(TclValidator.UNKNOWN_NAME)
	def createAMLMask(Issue issue, IssueResolutionAcceptor acceptor) {
		acceptor.accept(issue, "Create AML Mask", "Creates a new AML Mask", 'upcase.png') [ element, context |
			if (element instanceof ComponentTestStepContext) {
				if (element.component.eIsProxy) {
					val maskName = getMaskName(element)
					var amlFile = getTargetFile(context.xtextDocument.getAdapter(IFile))
					if (amlFile != null) {
						var editor = openEditorFor(amlFile)
						var amlModel = editor.document.readOnly() [ ressource |
							return ressource.contents.head as AmlModel
						]
						var ICompositeNode lastNode = null
						if (amlModel.components.empty) {
							lastNode = NodeModelUtils.findActualNodeFor(amlModel)
						} else {
							lastNode = NodeModelUtils.findActualNodeFor(amlModel.components.last)
						}
						editor.document.replace(lastNode.offset + lastNode.length, 0, getComponentDSLFragment(maskName))
					}
				}
			}
		]
	}

	def openEditorFor(IFile file) {
		var fei = new FileEditorInput(file)
		var id = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(file.getName()).id
		var editor = PlatformUI.workbench.activeWorkbenchWindow.activePage.openEditor(fei, id) as XtextEditor
		return editor
	}

	def getTargetFile(IFile currentSelection) {
		val dialog = new ElementTreeSelectionDialog(Display.getDefault().getActiveShell(), new WorkbenchLabelProvider(),
			new BaseWorkbenchContentProvider())
		dialog.input = ResourcesPlugin.getWorkspace().getRoot()
		dialog.allowMultiple = true
		dialog.title = "Select AML file"
		dialog.initialSelection = currentSelection.parent
		dialog.addFilter(new ViewerFilter() {

			override select(Viewer viewer, Object parentElement, Object element) {
				if (element instanceof IFile) {
					return element.toString().endsWith("aml")
				} else
					return true
			}

		})
		if (dialog.open == Window.OK) {
			return dialog.firstResult as IFile
		}
		return null
	}

	def getMaskName(TestStepContext testStepContext) {
		val sub = NodeModelUtils.findActualNodeFor(testStepContext).text.split(':')
		return sub.get(1).trim
	}

	def String getComponentDSLFragment(String maskName) '''
		component «maskName» is <TYPE> {
		
		}
	'''

	@Fix(TclSyntaxErrorMessageProvider.MISSING_TEST_DESCRIPTION)
	def void fixMissingTestDescription(Issue issue, IssueResolutionAcceptor acceptor) {
		val defaultTestDescription = '''
			* test step description
			'''
		// inserting the missing test step in the right order and position is non trivial.
		// the quick fix for incomplete "implements" of a tsl should provide that.
		acceptor.accept(issue, "Add missing test description", '''
			Add a test description that will reference 
			the respective test of the test specification
			''', null) [ context | context.xtextDocument.replace(issue.offset, 0, defaultTestDescription)]
	}

}
