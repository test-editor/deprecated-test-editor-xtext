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

	static val NO_SEVERITY = -1

	// only descriptors, need not be disposed => no cleanup necessary
	val errorIcon = WorkbenchImages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_ERROR)
	val warningIcon = WorkbenchImages.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_WARNING)

	def void fireLabelEvent(LabelProviderChangedEvent event) {
		// make sure that event is executed in ui thread
		Display.^default.asyncExec[fireLabelProviderChanged(event)]
	}

	override decorate(Object element, IDecoration decoration) {
		switch (element.maxSeverity) {
			case IMarker.SEVERITY_ERROR:
				decoration.addOverlay(errorIcon)
			case IMarker.SEVERITY_WARNING:
				decoration.addOverlay(warningIcon)
			case NO_SEVERITY: {
			} // ignore
		}
	}

	private def IResource getResourceForSeverityCalculation(Object element) {
		switch (element) {
			IClasspathEntry case element.path.toString.matches(".*/src/(test|java)/java"):
				// classpath entries need to be heeded because of TELabelProvider
				ResourcesPlugin.workspace.root.getFolder(element.path)
			IProject:
				// project should only provide status of src and its subfolders (excluding technical problems elsewhere)
				element.getFolder("/src")
			IResource:
				element
			default:
				null
		}
	}

	private def int getMaxSeverity(Object element) {
		val resource = element.getResourceForSeverityCalculation
		try {
			if (resource !== null) {
				return resource.findMaxProblemSeverity(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE)
			}
		} catch (CoreException ce) {
			// ignore (and return default)
		}
		return NO_SEVERITY
	}

}
