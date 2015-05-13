package org.testeditor.xmllibrary.dsl.importer.ui.wizard

import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.InputStream
import org.eclipse.core.runtime.IStatus
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.Status
import org.eclipse.jface.preference.FileFieldEditor
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Composite
import org.eclipse.ui.dialogs.WizardNewFileCreationPage

/**
 * Initial version of page created using new-plugin wizard, then converted to Xtend and modified.
 */
class ImportWizardPage extends WizardNewFileCreationPage {

	protected FileFieldEditor editor

	new(String pageName, IStructuredSelection selection) {
		super(pageName, selection)
		setTitle(pageName)
		setDescription("Import a file from the local file system into the workspace")
	}

	override protected void createAdvancedControls(Composite parent) {
		val composite = new Composite(parent, SWT.NONE) => [
			layoutData = new GridData(GridData.GRAB_HORIZONTAL.bitwiseOr(GridData.FILL_HORIZONTAL))
			layout = new GridLayout => [
				numColumns = 3
				makeColumnsEqualWidth = false
				marginWidth = 0
				marginHeight = 0
			]
		]
		editor = new FileFieldEditor("fileSelect", "Select File: ", composite) => [
			getTextControl(composite).addModifyListener[
				val path = new Path(editor.stringValue)
				fileName = path.lastSegment
			]
			fileExtensions = #["*.xml"]
		]

		composite.moveAbove(null)
	}

	override protected void createLinkTarget() {
	}

	override protected InputStream getInitialContents() {
		try {
			return new FileInputStream(new File(editor.stringValue))
		} catch (FileNotFoundException e) {
			return null
		}

	}

	override protected String getNewFileLabel() {
		return "New File Name:"
	}

	override protected IStatus validateLinkedResource() {
		return new Status(IStatus.OK, "org.testeditor.xmllibrary.dsl.importer.ui", IStatus.OK, "", null)
	}

}
