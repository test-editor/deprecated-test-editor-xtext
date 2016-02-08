package org.testeditor.rcp4.views

import org.eclipse.jface.viewers.ITreeContentProvider
import org.eclipse.jface.viewers.Viewer
import java.io.File

class MaskStepSelectorTreeContentProvider implements ITreeContentProvider  {
	
	var MaskStepSelectorInput input
	
	override getChildren(Object parentElement) {
		// input.getChildrenOf(parentElement as EObject)
		(parentElement as File).listFiles
	}
	
	override getElements(Object inputElement) {
		inputElement as File[]
	}
	
	override getParent(Object element) {
		(element as File).parentFile
	}
	
	override hasChildren(Object element) {
		(element as File).isDirectory
	}
	
	override dispose() {
	}
	
	override inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}
	
}