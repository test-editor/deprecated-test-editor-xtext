package org.testeditor.aml.legacy.importer.ui

import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.commands.ExecutionEvent
import org.eclipse.core.commands.ExecutionException
import org.eclipse.xtext.ui.editor.utils.EditorUtils
import org.eclipse.xtext.ui.util.ClipboardUtil

class PasteXmlHandler extends AbstractHandler {
	
	override execute(ExecutionEvent event) throws ExecutionException {
		val activeEditor = EditorUtils.getActiveXtextEditor(event)
		if (activeEditor === null) {
			return null
		}
		
		val clipboardText = ClipboardUtil.textFromClipboard
		
		println(clipboardText)
		
		return null
	}
	
}