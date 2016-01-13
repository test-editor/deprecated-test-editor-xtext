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
package org.testeditor.aml.legacy.importer.ui

import javax.inject.Inject
import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.commands.ExecutionEvent
import org.eclipse.core.commands.ExecutionException
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.serializer.ISerializer
import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.xtext.ui.editor.model.IXtextDocument
import org.eclipse.xtext.ui.editor.utils.EditorUtils
import org.eclipse.xtext.ui.util.ClipboardUtil
import org.testeditor.aml.legacy.importer.XmlToAmlConverter
import org.testeditor.aml.AmlModel

import static java.lang.System.lineSeparator
import static org.eclipse.xtext.nodemodel.util.NodeModelUtils.*
import org.eclipse.jface.text.source.ISourceViewer

/**
 * Handler that converts the old legacy XML into the AML syntax and pastes
 * the result into the open editor.
 * 
 * Inspired by {@code org.eclipse.xtend.ide.javaconverter.PasteJavaCodeHandler}
 */
class PasteXmlHandler extends AbstractHandler {
	
	static val doubleLineSeparator = lineSeparator + lineSeparator
	
	@Inject
	XmlToAmlConverter converter
	
	@Inject
	ISerializer serializer
	
	override execute(ExecutionEvent event) throws ExecutionException {
		val activeEditor = EditorUtils.getActiveXtextEditor(event)
		if (activeEditor === null) {
			return null
		}
		
		val clipboardText = ClipboardUtil.textFromClipboard
		if (!clipboardText.nullOrEmpty) {
			doPasteXml(activeEditor, clipboardText)
		}
		
		return null
	}
	
	protected def doPasteXml(XtextEditor editor, String xml) {
		// Convert the XML
		val converted = xml.convert
		
		// Find the proper place to paste it
		val document = editor.document
		val targetElement = editor.getTargetElement(document)
		val node = findActualNodeFor(targetElement)
		
		// Paste the converted XML
		val offset = node.offset + node.length
		val convertedAml = doubleLineSeparator + converted
		document.replace(offset, 0, convertedAml)	
		
		// Trigger formatting for pasted contents
		editor.doFormat(offset, convertedAml)
	}
	
	protected def String convert(String xml) {
		val elements = converter.convert(xml)
		val result = elements.map[serializer.serialize(it)].join(doubleLineSeparator)
		return result
	}
	
	/**
	 * Finds the element into which we would theoretically paste. This might be a nested element,
	 * in this case find the parent that is directly below {@link AmlModel}.
	 */
	protected def EObject getTargetElement(XtextEditor editor, IXtextDocument document) {
		val sourceViewer = editor.internalSourceViewer
		val selectionOffset = sourceViewer.selectedRange.x - 1
		var targetElement = document.readOnly[ resource |
			val parseResult = resource.parseResult
			if (parseResult === null) {
				return null
			}
			val leafNode = findLeafNodeAtOffset(parseResult.rootNode, selectionOffset)
			return leafNode.semanticElement
		]
		while (targetElement.eContainer !== null && !(targetElement.eContainer instanceof AmlModel)) {
			targetElement = targetElement.eContainer
		}
		return targetElement
	}
	
	protected def void doFormat(XtextEditor editor, int offset, String amlString) {
		val sourceViewer = editor.internalSourceViewer
		val length = amlString.length
		sourceViewer.setSelectedRange(offset, length)
		sourceViewer.textOperationTarget.doOperation(ISourceViewer.FORMAT)
		val restoreCaretAtOffset = sourceViewer.selectedRange.x + sourceViewer.selectedRange.y
		sourceViewer.setSelectedRange(restoreCaretAtOffset, 0)
	}
	
}