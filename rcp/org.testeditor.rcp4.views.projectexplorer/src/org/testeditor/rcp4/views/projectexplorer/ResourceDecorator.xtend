package org.testeditor.rcp4.views.projectexplorer

import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.CoreException
import org.eclipse.jdt.core.IClasspathEntry
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
		try {
			val maxSeverity = if (element instanceof IClasspathEntry) {
					if (element.path.toString.matches(".*/src/(test|java)/java")) {
						ResourcesPlugin.workspace.root.getFolder(element.path).findMaxProblemSeverity(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE)						
					}else {
						IMarker.SEVERITY_INFO
					}
				} else if (element instanceof IProject) {
					element.getFolder("/src").findMaxProblemSeverity(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE)
				} else if (element instanceof IResource) {
					element.findMaxProblemSeverity(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE)
				} else {
					IMarker.SEVERITY_INFO 
				}
			val errorIcon = WorkbenchImages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_ERROR)
			val warningIcon = WorkbenchImages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_WARNING)
			switch (maxSeverity) {
				case IMarker.SEVERITY_ERROR: decoration.addOverlay(errorIcon)
				case IMarker.SEVERITY_WARNING: decoration.addOverlay(warningIcon)
			// default: // do nothing		
			}
		} catch (CoreException ce) {
			// ignore
		}
	}

}
