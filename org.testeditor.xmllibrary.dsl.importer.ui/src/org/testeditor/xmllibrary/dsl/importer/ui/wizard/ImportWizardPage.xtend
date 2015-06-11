package org.testeditor.xmllibrary.dsl.importer.ui.wizard

import java.io.File
import java.io.InputStream
import javax.inject.Inject
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
import org.testeditor.xmllibrary.domain.action.ActionGroups
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes
import org.testeditor.xmllibrary.dsl.importer.ui.Activator
import org.testeditor.xmllibrary.dsl.importer.xml.Converter
import org.testeditor.xmllibrary.dsl.importer.xml.JAXBHelper

import static extension org.eclipse.jface.dialogs.MessageDialog.*
import org.eclipse.xtext.util.StringInputStream

/**
 * Initial version of page created using new-plugin wizard, then converted to Xtend and modified.
 */
class ImportWizardPage extends WizardNewFileCreationPage {

	protected FileFieldEditor editor
	
	@Inject JAXBHelper jaxbHelper
	@Inject Converter converter

	new(String pageName, IStructuredSelection selection) {
		super(pageName, selection)
		title = pageName
		description = "Import an existing AllActionGroups / TechnicalBindings XML file from the local file system"
		Activator.getDefault.injector.injectMembers(this)
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
			getTextControl(composite).addModifyListener [
				val newFileName = editor.stringValue.newFileName
				if (!newFileName.nullOrEmpty) {
					fileName = newFileName
				}
			]
			fileExtensions = #["*.xml"]
		]

		composite.moveAbove(null)
	}

	protected def String getNewFileName(String editorValue) {
		if(editorValue.nullOrEmpty) return null
		val path = new Path(editorValue)
		val file = path.toFile
		if(!file.exists) return null

		val fileName = path.removeFileExtension.lastSegment
		val unmarshaller = jaxbHelper.createUnmarshaller
		try {
			val object = unmarshaller.unmarshal(file)
			if (object instanceof ActionGroups) {
				return fileName + ".agdsl"
			} else if (object instanceof TechnicalBindingTypes) {
				return fileName + ".tbdsl"
			} else {
				shell.openError("Error", "Unrecognized object type: " + object);
			}
		} catch (Exception e) {
			shell.openError("Error", '''
			Error parsing file: "«path»"
			Did you select a valid AllActionGroups / TechnicalBindings file?
			
			Exception:
			«e.toString»''')
		}
		return null
	}

	override protected void createLinkTarget() {
	}

	override protected InputStream getInitialContents() {
		try {
			val file = new File(editor.stringValue)
			val unmarshaller = jaxbHelper.createUnmarshaller
			val object = unmarshaller.unmarshal(file)
			val string = converter.convert(object)
			return new StringInputStream(string)
		} catch (Exception e) {
			shell.openError("Error", '''
			Caught exception:
			«e.toString»
			''')
			e.printStackTrace
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
