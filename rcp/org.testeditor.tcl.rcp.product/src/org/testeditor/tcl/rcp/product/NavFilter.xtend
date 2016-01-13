package org.testeditor.tcl.rcp.product

import org.eclipse.jdt.internal.core.JavaElement
import org.eclipse.jdt.internal.ui.packageview.ClassPathContainer
import org.eclipse.jface.viewers.Viewer
import org.eclipse.jface.viewers.ViewerFilter

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
