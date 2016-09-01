package org.testeditor.dsl.common.ui.refactoring

import com.google.common.annotations.VisibleForTesting
import java.util.Set
import javax.inject.Inject
import javax.inject.Named
import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.OperationCanceledException
import org.eclipse.ltk.core.refactoring.Change
import org.eclipse.ltk.core.refactoring.TextFileChange
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext
import org.eclipse.ltk.core.refactoring.participants.RenameParticipant
import org.eclipse.text.edits.ReplaceEdit
import org.eclipse.ui.part.FileEditorInput
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.ui.editor.model.IXtextDocument
import org.eclipse.xtext.ui.editor.model.XtextDocumentProvider
import org.eclipse.xtext.ui.refactoring.impl.EditorDocumentChange
import org.eclipse.xtext.ui.resource.IResourceSetProvider
import org.testeditor.dsl.common.NamedElement
import org.testeditor.dsl.common.ui.workbench.PartHelper

import static org.eclipse.xtext.Constants.*
import static org.testeditor.dsl.common.CommonPackage.Literals.*

import static extension org.eclipse.xtext.nodemodel.util.NodeModelUtils.findNodesForFeature

@Accessors(PROTECTED_GETTER) // for testing
class NamedElementRenameParticipant extends RenameParticipant {

	@Inject PartHelper partHelper
	@Inject IResourceSetProvider resourceSetProvider
	@Inject XtextDocumentProvider documentProvider

	val Set<String> fileExtensions
	@Accessors(PUBLIC_GETTER) val String name
	IFile element

	@Inject
	new(@Named(FILE_EXTENSIONS) String fileExtensions, @Named(LANGUAGE_NAME) String languageName) {
		this.fileExtensions = fileExtensions.split(",").map[toLowerCase].toSet
		this.name = languageName.split("\\.").last + " Renaming"
	}

	override checkConditions(IProgressMonitor pm, CheckConditionsContext context) throws OperationCanceledException {
		// TODO check the validity of the new name
	}

	/** 
	 * Creates the change for the name refactoring.
	 * We need to handle the cases when an element is opened in an editor and not separately.
	 */
	override createPreChange(IProgressMonitor pm) throws CoreException, OperationCanceledException {
		val editor = partHelper.findEditor(element)
		if (editor.present) {
			return createEditorChange(editor.get as XtextEditor)
		} else {
			return createTextFileChange
		}
	}

	override createChange(IProgressMonitor pm) throws CoreException, OperationCanceledException {
		// do nothing here, we want to create "pre changes" as we want the name refactoring
		// to happen before the resource is moved
		return null
	}

	/**
	 * Creates a name replacement for a resource with an open editor.
	 */
	private def Change createEditorChange(XtextEditor editor) {
		val namedElement = getNamedElement(editor.document)
		val nameReplacement = createNameReplacement(namedElement)
		val shouldSave = !editor.isDirty
		return new EditorDocumentChange("Change name", editor, shouldSave) => [
			edit = nameReplacement
			textType = element.fileExtension
		]
	}

	/**
	 * Creates a name replacement for a resource with no open editor.
	 */
	private def Change createTextFileChange() {
		// retrieve the document
		val input = new FileEditorInput(element)
		documentProvider.connect(input)
		try {
			val document = documentProvider.getDocument(input) as IXtextDocument
			val namedElement = getNamedElement(document)
			val nameReplacement = createNameReplacement(namedElement)

			// create the change
			return new TextFileChange("Change name", element) => [
				saveMode = TextFileChange.FORCE_SAVE
				edit = nameReplacement
				textType = element.fileExtension
			]
		} finally {
			documentProvider.disconnect(input)
		}
	}

	@VisibleForTesting
	protected def ReplaceEdit createNameReplacement(NamedElement namedElement) {
		val node = namedElement.findNodesForFeature(NAMED_ELEMENT__NAME).head
		val newName = arguments.newName.replaceFirst('''\.(«fileExtensions.join("|")»)''', "")
		return new ReplaceEdit(node.offset, node.length, newName)
	}

	@VisibleForTesting
	protected def NamedElement getNamedElement(IXtextDocument document) {
		return document.readOnly [ resource |
			resource.allContents.filter(NamedElement).head
		]
	}

	/**
	 * @return {@code true} if the passed element is a file and has the right file extension
	 */
	override protected initialize(Object element) {
		if (element instanceof IFile) {
			val fileExtension = element.fileExtension?.toLowerCase
			if (fileExtension !== null && fileExtensions.contains(fileExtension)) {
				this.element = element
				return true
			}
		}
		return false
	}

}
