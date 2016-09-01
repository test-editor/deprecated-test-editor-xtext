package org.testeditor.dsl.common.ui.refactoring

import com.google.inject.Module
import com.google.inject.name.Names
import java.util.List
import java.util.Optional
import javax.inject.Inject
import org.apache.commons.lang3.reflect.FieldUtils
import org.eclipse.jface.text.IDocumentPartitioner
import org.eclipse.ltk.core.refactoring.TextFileChange
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor
import org.eclipse.ltk.core.refactoring.participants.RenameArguments
import org.eclipse.text.edits.ReplaceEdit
import org.eclipse.xtext.ide.LexerIdeBindings
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.parser.antlr.Lexer
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.ui.editor.model.IResourceForEditorInputFactory
import org.eclipse.xtext.ui.editor.model.IXtextDocument
import org.eclipse.xtext.ui.editor.model.XtextDocument
import org.eclipse.xtext.ui.editor.model.XtextDocumentProvider
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionProvider
import org.eclipse.xtext.ui.refactoring.impl.EditorDocumentChange
import org.eclipse.xtext.ui.resource.IResourceSetProvider
import org.eclipse.xtext.ui.resource.IStorage2UriMapper
import org.eclipse.xtext.util.concurrent.IUnitOfWork
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.testeditor.dsl.common.NamedElement
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.dsl.common.testing.eclipse.ResourceMocker
import org.testeditor.dsl.common.ui.workbench.PartHelper
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.TclRuntimeModule

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*

/**
 * Unit test for {@link NamedElementRenameParticipant}.
 * 
 * Uses the Tcl language for convenience.
 */
class NamedElementRenameParticipantTest extends AbstractTest {

	static val oldName = "MyTest"
	static val newName = "NewTest"

	@Inject NamedElementRenameParticipant participant
	@Mock PartHelper partHelper
	@Mock IResourceSetProvider resourceSetProvider
	@Mock XtextDocumentProvider documentProvider

	@Inject extension ResourceMocker
	@Inject ParseHelper<TclModel> parseHelper

	override protected collectModules(List<Module> modules) {
		super.collectModules(modules)
		modules += new TclRuntimeModule
		modules += [ binder |
			binder.bind(PartHelper).toInstance(partHelper)
			binder.bind(IResourceSetProvider).toInstance(resourceSetProvider)
			binder.bind(XtextDocumentProvider).toInstance(documentProvider)

			// make Guice happy
			binder.bind(Lexer).annotatedWith(Names.named(LexerIdeBindings.HIGHLIGHTING)).toInstance(Lexer.mock)
			binder.bind(IDocumentPartitioner).toInstance(IDocumentPartitioner.mock)
			binder.bind(IssueResolutionProvider).toInstance(IssueResolutionProvider.mock)
			binder.bind(IResourceForEditorInputFactory).toInstance(IResourceForEditorInputFactory.mock)
			binder.bind(IStorage2UriMapper).toInstance(IStorage2UriMapper.mock)
		]
	}

	@Before
	def void performDefaultInitialiatzion() {
		val file = mockFile(oldName + '.tcl')
		val processor = RefactoringProcessor.mock
		val arguments = new RenameArguments(newName, false)
		participant.initialize(processor, file, arguments)
	}

	@Test
	def void nameIsProperlyReturned() {
		// expect
		participant.name.assertEquals("Tcl Renaming")
	}

	@Test
	def void fileExtensionsArePopulated() {
		// expect
		participant.fileExtensions.assertEquals(#["tcl", "tml", "config"].toSet)
	}

	@Test
	def void initializesWithProperFileExtension() {
		// given
		val file = mockFile("myFile.tcl")

		// when
		val result = participant.initialize(file)

		// then
		result.assertTrue
		participant.element.assertSame(file)
	}

	@Test
	def void initializesWithUpperCaseFileExtension() {
		// given
		val file = mockFile("myFile.TCL")

		// when
		val result = participant.initialize(file)

		// then
		result.assertTrue
		participant.element.assertSame(file)
	}

	@Test
	def void doesNotInitializeWithSomeOtherFile() {
		// given
		val file = mockFile("myFile.unrelated")

		// when
		val result = participant.initialize(file)

		// then
		result.assertFalse
	}

	@Test
	def void doesNotInitializeWithDirectory() {
		// given
		val file = mockFile("myDirectory")

		// when
		val result = participant.initialize(file)

		// then
		result.assertFalse
	}

	@Test
	def void doesNotInitializeWithSomeOtherObject() {
		// when
		val result = participant.initialize("someOtherObject")

		// then
		result.assertFalse
	}

	/**
	 * Participant should create "pre changes" and return {@code null} on createChange
	 */
	@Test
	def void producesEmptyChange() {
		// expect
		participant.createChange(null).assertNull
	}

	@Test
	def void namedElementIsRetrievedProperly() {
		// given
		val document = mockDocument

		// when
		val namedElement = participant.getNamedElement(document)

		// then
		namedElement.assertNotNull
		namedElement.name.assertEquals("MyTest")
	}

	@Test
	def void nameReplacementIsCorrect() {
		// given
		val document = mockDocument
		val namedElement = participant.getNamedElement(document)

		// when
		val replacement = participant.createNameReplacement(namedElement)

		// then
		replacement => [
			val text = document.get
			offset.assertSame(text.indexOf(oldName))
			length.assertSame(oldName.length)
			getText.assertEquals(newName)
		]
	}

	@Test
	def void editorChangeWhenEditorIsDirty() {
		// given
		val editor = mockEditor(true)

		// when
		val change = participant.createPreChange(null)

		// then
		change.assertInstanceOf(EditorDocumentChange) => [
			getEditor.assertSame(editor)
			doSave.assertFalse // editor is dirty => change should NOT be saved
			textType.assertEquals('tcl')
			edit.assertInstanceOf(ReplaceEdit)
		]
	}

	@Test
	def void editorChangeWhenEditorIsNotDirty() {
		// given
		val editor = mockEditor(false)

		// when
		val change = participant.createPreChange(null)

		// then
		change.assertInstanceOf(EditorDocumentChange) => [
			getEditor.assertSame(editor)
			doSave.assertTrue // editor is not dirty => change should be saved
			textType.assertEquals('tcl')
			edit.assertInstanceOf(ReplaceEdit)
		]
	}

	@Test
	def void textChangeCreated() {
		// given
		val document = mockDocument
		when(documentProvider.getDocument(any)).thenReturn(document)
		when(partHelper.findEditor(participant.element)).thenReturn(Optional.empty)
		// need to write the private field of Mock due to unmockable final method connect(...) that accesses it
		FieldUtils.writeField(documentProvider, "fElementInfoMap", newHashMap, true)

		// when
		val change = participant.createPreChange(null)

		// then
		change.assertInstanceOf(TextFileChange) => [
			saveMode.assertEquals(TextFileChange.FORCE_SAVE)
			textType.assertEquals('tcl')
			edit.assertInstanceOf(ReplaceEdit)
		]
	}

	private def XtextEditor mockEditor(boolean dirty) {
		val document = mockDocument
		val editor = XtextEditor.mock(RETURNS_DEEP_STUBS) => [
			when(it.document).thenReturn(document)
			when(it.isDirty).thenReturn(dirty)
			when(it.documentProvider.getDocument(any)).thenReturn(document)
		]
		when(partHelper.findEditor(participant.element)).thenReturn(Optional.of(editor))
		return editor
	}

	private def IXtextDocument mockDocument() {
		// Parse a TCL model in order to get a real Xtext resource
		val documentText = '''
			package com.example
			
			# «oldName»
		'''
		val model = parseHelper.parse(documentText)
		val resource = model.eResource.assertInstanceOf(XtextResource)

		val document = XtextDocument.mock => [
			when(<NamedElement>readOnly(any)).thenAnswer [
				val closure = getArgumentAt(0, IUnitOfWork)
				return closure.exec(resource)
			]
			when(get).thenReturn(documentText)
		]
		return document
	}

}
