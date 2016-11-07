package org.testeditor.rcp4.views.projectexplorer

import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.jface.viewers.DecorationOverlayIcon
import org.eclipse.jface.viewers.ILabelDecorator
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.viewers.LabelProviderChangedEvent
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.ISharedImages
import org.eclipse.ui.internal.WorkbenchImages

class ResourceDecorator extends LabelProvider implements ILabelDecorator {

	override decorateImage(Image baseImage, Object element) {
		if (element instanceof IResource) try {
			val maxSeverity = element.findMaxProblemSeverity(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
			val errorIcon=WorkbenchImages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_ERROR)
			val warningIcon=WorkbenchImages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_WARNING)
			switch(maxSeverity){
				case IMarker.SEVERITY_ERROR: return new DecorationOverlayIcon(baseImage, #[errorIcon]).createImage
				case IMarker.SEVERITY_WARNING: return new DecorationOverlayIcon(baseImage, #[warningIcon]).createImage
				default: return null		
			}
		}catch(CoreException ce){
			// ignore
		}
		return null
	}

	override decorateText(String text, Object element) {
		return null
	}

	def void fireLabelEvent(LabelProviderChangedEvent event) {
		Display.^default.asyncExec[fireLabelProviderChanged(event)]
	}
}
