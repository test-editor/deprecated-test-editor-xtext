package org.testeditor.dsl.common.ui.workbench

import org.eclipse.e4.ui.model.application.ui.MUIElement
import java.util.LinkedList
import org.eclipse.e4.ui.model.application.ui.MElementContainer

class MUIElementUtils {
	
	/** return all elements within the tree flattened to an iterable (depth-first order) */
	def Iterable<MUIElement> flattenTree(MUIElement element) {
		val result = newLinkedList
		element.flattenTreeInto(result)
		return result
	}
	
	private def Iterable<MUIElement> flattenTreeInto(MUIElement element, LinkedList<MUIElement> list) {
		list.add(element)
		if (element instanceof MElementContainer<?>) {
			element.children.forEach[flattenTreeInto(list)]
		}

		return list
	}

}