package org.testeditor.rcp4.views.projectexplorer

import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.jface.viewers.IDecoration
import org.eclipse.jface.viewers.ILightweightLabelDecorator
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.viewers.LabelProviderChangedEvent
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.ISharedImages
import org.eclipse.ui.internal.WorkbenchImages

class ResourceDecorator extends LabelProvider implements ILightweightLabelDecorator {

	def void fireLabelEvent(LabelProviderChangedEvent event) {
		Display.^default.asyncExec[fireLabelProviderChanged(event)]
	}
	
	override decorate(Object element, IDecoration decoration) {
		if (element instanceof IResource) try {
			val maxSeverity = element.findMaxProblemSeverity(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
			val errorIcon=WorkbenchImages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_ERROR)
			val warningIcon=WorkbenchImages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_WARNING)
			switch(maxSeverity){
				case IMarker.SEVERITY_ERROR: decoration.addOverlay(errorIcon) 
				case IMarker.SEVERITY_WARNING: decoration.addOverlay(warningIcon)
				// default: // do nothing		
			}
		}catch(CoreException ce){
			// ignore
		}
	}
	
}
