package org.testeditor.rcp4.views.projectexplorer

import javax.inject.Inject
import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IPath
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.jface.viewers.IDecoration
import org.eclipse.jface.viewers.ILightweightLabelDecorator
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.viewers.LabelProviderChangedEvent
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.ISharedImages
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.utils.WorkbenchImagesHelper
import org.testeditor.dsl.common.util.WorkspaceHelper
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

class ResourceDecorator extends LabelProvider implements ILightweightLabelDecorator {

	static val logger = LoggerFactory.getLogger(ResourceDecorator)

	@Inject WorkspaceHelper workspaceHelper
	@Inject WorkbenchImagesHelper imagesHelper
	@Inject ClasspathUtil classpathUtil

	static val NO_SEVERITY = -1

	def void fireLabelEvent(LabelProviderChangedEvent event) {
		// make sure that event is executed in ui thread
		Display.^default.asyncExec[fireLabelProviderChanged(event)]
	}

	override decorate(Object element, IDecoration decoration) {
		try {
			switch (element.maxSeverity) {
				case IMarker.SEVERITY_ERROR:
					decoration.addOverlay(errorIcon, IDecoration.BOTTOM_LEFT)
				case IMarker.SEVERITY_WARNING:
					decoration.addOverlay(warningIcon, IDecoration.BOTTOM_LEFT)
				case IMarker.SEVERITY_INFO:
					decoration.addOverlay(infoIcon, IDecoration.BOTTOM_LEFT)
				default: {
				} // ignore, that is no decoration
			}
		} catch (Exception e) {
			logger.error("Exception during decoration process.", e)
		}
	}

	private def Iterable<IResource> getResourcesForSeverityCalculation(Object element) {
		switch (element) {
			IClasspathEntry case element.entryKind == IClasspathEntry.CPE_SOURCE:
				// classpath entries need to be heeded because of TELabelProvider
				return #[getResourcesFor(element.path)].filterNull
			IProject:
				// project should only provide status of its source folders and its subfolders (excluding technical problems elsewhere)
				return classpathUtil.getSourceClasspathEntries(element).map[getResourcesFor(path)].filterNull
			IResource:
				return #[element]
			default:
				return emptyList
		}
	}

	/** may return null */
	private def IResource getResourcesFor(IPath path) {
		try {
			return workspaceHelper.root.getFolder(path)
		} catch (IllegalArgumentException e) { // if path cannot be resolved: this may happen during creation/import of a new project
			return null
		}
	}

	private def int getMaxSeverity(Object element) {
		val resources = element.resourcesForSeverityCalculation
		if (! resources.empty) {
			val severities = resources.map [
				try {
					return findMaxProblemSeverity(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE)
				} catch (CoreException ce) {
					return NO_SEVERITY
				}
			]
			return severities.max
		} else {
			return NO_SEVERITY
		}
	}

	// only descriptors, need not be disposed => no cleanup necessary
	private def ImageDescriptor getErrorIcon() {
		return imagesHelper.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_ERROR)
	}

	private def ImageDescriptor getWarningIcon() {
		return imagesHelper.getImageDescriptor(ISharedImages.IMG_DEC_FIELD_WARNING)
	}
	
	private def ImageDescriptor getInfoIcon() {
		return ImageDescriptor.createFromFile(ResourceDecorator,"/icons/info_ovr.gif")
	}

}
