package org.testeditor.dsl.common.ui.workbench

import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.e4.ui.model.application.ui.MElementContainer
import org.eclipse.e4.ui.model.application.ui.MUIElement

@Creatable
class MUIElementUtils {

	/** return all elements within the tree flattened to an iterable (order not guaranteed) */
	def Iterable<MUIElement> flattenTree(MUIElement element) {
		val result = newLinkedList
		result.add(element)
		if (element instanceof MElementContainer<?>) {
			element.children.forEach[result.addAll(flattenTree)]
		}
		return result
	}

}
