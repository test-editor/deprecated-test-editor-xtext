package org.testeditor.dsl.common.util

import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.ui.internal.WorkbenchImages

class WorkbenchImagesHelper {
	
	public def ImageDescriptor getImageDescriptor(String id) {
		return WorkbenchImages.getImageDescriptor(id)
	}
	
}