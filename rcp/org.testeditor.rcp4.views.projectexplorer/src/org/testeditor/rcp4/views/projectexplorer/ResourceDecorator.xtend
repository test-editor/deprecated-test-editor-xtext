package org.testeditor.rcp4.views.projectexplorer

import javax.inject.Inject
import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IPath
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jface.viewers.IDecoration
import org.eclipse.jface.viewers.ILightweightLabelDecorator
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.viewers.LabelProviderChangedEvent
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.ISharedImages
import org.eclipse.ui.internal.WorkbenchImages
import org.testeditor.dsl.common.util.WorkspaceHelper

class ResourceDecorator extends LabelProvider implements ILightweightLabelDecorator {

	@Inject WorkspaceHelper workspaceHelper

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
			default: {
			} // ignore, that is no decoration
		}
	}

	private def Iterable<IResource> getResourcesForSeverityCalculation(Object element) {
		switch (element) {
			IClasspathEntry case element.entryKind==IClasspathEntry.CPE_SOURCE:
				// classpath entries need to be heeded because of TELabelProvider
 				return #[getResourceFor(element.path)]
			IProject:
				// project should only provide status of its source folders and its subfolders (excluding technical problems elsewhere)
				getJavaSourceClasspaths(element).map[getResourceFor(path)]
			IResource:
				return #[element]
			default:
				return emptyList
		}
	}

	private def Iterable<IClasspathEntry> getJavaSourceClasspaths(IProject project) {
		if (JavaProject.hasJavaNature(project)) {
			return JavaCore.create(project).rawClasspath.filter[entryKind == IClasspathEntry.CPE_SOURCE]
		} else {
			return emptyList
		}
	}
	
	private def IResource getResourceFor(IPath path) {
		return workspaceHelper.root.getFolder(path)
	}

	private def int getMaxSeverity(Object element) {
		val resources = element.resourcesForSeverityCalculation
		try {
			val severities = resources.map[findMaxProblemSeverity(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE)]
			val maxSeverity = severities.reduce[a, b|Math.max(a, b)]
			return maxSeverity?:NO_SEVERITY
		} catch (CoreException ce) {
			// ignore (and return default)
		}
		return NO_SEVERITY
	}

}
