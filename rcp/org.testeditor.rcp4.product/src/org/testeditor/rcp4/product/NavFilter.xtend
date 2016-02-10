package org.testeditor.rcp4.product

import org.eclipse.jdt.internal.core.JavaElement
import org.eclipse.jdt.internal.ui.packageview.ClassPathContainer
import org.eclipse.jface.viewers.Viewer
import org.eclipse.jface.viewers.ViewerFilter

/** filter all classpath containers (Java RT, JUnit) and java elements from navigator
 * 
 *  thus the user of the rcp sees only the files relevant to him in the project explorer
 */
class NavFilter extends ViewerFilter {
	override select(Viewer viewer, Object parentElement, Object element) {
		if (element instanceof JavaElement) {
			return false
		}
		if (element instanceof ClassPathContainer) {
			return false
		}
		return true
	}
}
