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
package org.testeditor.aml.dsl.ui.wizard

import org.eclipse.core.resources.IContainer
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.Path
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.wizard.WizardPage
import org.eclipse.swt.SWT
import org.eclipse.swt.events.SelectionAdapter
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Text
import org.eclipse.ui.dialogs.ContainerSelectionDialog

/** 
 * The "New" wizard page allows setting the container for the new file as well
 * as the file name. The page will only accept file name without the extension
 * OR with the extension that matches the expected one (aml).
 */
class AmlNewFileWizardPage extends WizardPage {

	Text containerText
	Text fileText
	ISelection selection

	/** 
	 * Constructor for SampleNewWizardPage.
	 * @param pageName
	 */
	new(ISelection selection) {
		super("wizardPage")
		title = "New Aml File"
		description = "This wizard creates a new file with *.aml extension."
		this.selection = selection
	}

	override void createControl(Composite parent) {
		val container = new Composite(parent, SWT.NULL) => [
			layout = new GridLayout => [
				numColumns = 3
				verticalSpacing = 9
			]
		]
		new Label(container, SWT.NULL) => [
			text = "&Container:"
		]
		containerText = new Text(container, SWT.BORDER.bitwiseOr(SWT.SINGLE)) => [
			layoutData = new GridData(GridData.FILL_HORIZONTAL)
			addModifyListener[dialogChanged]
		]
		new Button(container, SWT.PUSH) => [
			text = "Browse..."
			addSelectionListener(new SelectionAdapter() {
				override void widgetSelected(SelectionEvent e) {
					handleBrowse
				}
			})
		]
		new Label(container, SWT.NULL) => [
			text = "&File name:"
		]
		fileText = new Text(container, SWT.BORDER.bitwiseOr(SWT.SINGLE)) => [
			layoutData = new GridData(GridData.FILL_HORIZONTAL)
			addModifyListener[dialogChanged]
		]
		initialize
		dialogChanged
		setControl(container)
	}

	/** 
	 * Tests if the current workbench selection is a suitable container to use.
	 */
	def private void initialize() {
		if (selection instanceof IStructuredSelection) {
			if(selection.size !== 1) return
			val obj = selection.firstElement
			if (obj instanceof IResource) {
				val container = if (obj instanceof IContainer) obj else obj.parent
				containerText.text = container.fullPath.toString
				setFileText(container.name)
			}
			if (obj instanceof IJavaElement) {
				val parentPackage = obj.getAncestor(IJavaElement.PACKAGE_FRAGMENT)
				if (parentPackage !== null) {
					containerText.text = parentPackage.resource.fullPathString	
					setFileText(parentPackage.path.lastSegment) // elementName is fully qualified, don't want that
				} else {
					val project = obj.javaProject
					containerText.text = project?.sourceFolderPath
					setFileText(project?.elementName)
				}
			}
		}
		if (fileText.text.nullOrEmpty) {
			setFileText("demo")
		}
	}
	
	def private setFileText(String name) {
		if (!name.isNullOrEmpty) {
			fileText.text = name + ".aml"
		}
	}
	
	def private getSourceFolderPath(IJavaProject javaProject) {
		val srcFolder = javaProject.project.getFolder(AmlProjectCreator.SRC_ROOT)
		return srcFolder.fullPathString
	}

	def private getFullPathString(IResource resource) {
		return resource?.fullPath?.toString
	}

	/** 
	 * Uses the standard container selection dialog to choose the new value for
	 * the container field.
	 */
	def private void handleBrowse() {
		val dialog = new ContainerSelectionDialog(shell, ResourcesPlugin.workspace.root, false,
			"Select new file container")
		if (dialog.open === ContainerSelectionDialog.OK) {
			val result = dialog.result
			if (result.length === 1) {
				containerText.text = (result.head as Path).toString
			}
		}
	}

	/** 
	 * Ensures that both text fields are set.
	 */
	def private void dialogChanged() {
		val container = ResourcesPlugin.workspace.root.findMember(new Path(containerName))
		val fileName = getFileName
		if (containerName.nullOrEmpty) {
			updateStatus("File container must be specified")
			return
		}
		if (container === null ||
			(container.getType().bitwiseAnd((IResource.PROJECT.bitwiseOr(IResource.FOLDER)))) === 0) {
			updateStatus("File container must exist")
			return
		}
		if (!container.isAccessible()) {
			updateStatus("Project must be writable")
			return
		}
		if (fileName.length() === 0) {
			updateStatus("File name must be specified")
			return
		}
		if (fileName.replace(Character.valueOf('\\').charValue, Character.valueOf('/').charValue).indexOf(
			Character.valueOf('/').charValue, 1) > 0) {
			updateStatus("File name must be valid")
			return
		}
		var int dotLoc = fileName.lastIndexOf(Character.valueOf('.').charValue)
		if (dotLoc !== -1) {
			var String ext = fileName.substring(dotLoc + 1)
			if (ext.equalsIgnoreCase("aml") === false) {
				updateStatus("File extension must be \"aml\"")
				return
			}
		}
		val file = (container as IContainer).getFile(new Path(fileName))
		if (file.exists) {
			updateStatus("File already exists")
			return
		}
		updateStatus(null)
	}

	def private void updateStatus(String message) {
		errorMessage = message
		pageComplete = (message === null)
	}

	def String getContainerName() {
		return containerText.text
	}

	def String getFileName() {
		val fileName = fileText.text
		if (fileName.endsWith(".aml")) {
			return fileName
		} else {
			return fileName + ".aml"
		}
	}

	override setVisible(boolean visible) {
		super.setVisible(visible)
		if (visible) {
			if (!containerText.text.nullOrEmpty) {
				fileText.setFocus
			}
		}
	}

}