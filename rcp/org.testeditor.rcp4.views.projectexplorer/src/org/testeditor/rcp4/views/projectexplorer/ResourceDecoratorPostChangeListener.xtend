package org.testeditor.rcp4.views.projectexplorer

import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResourceChangeEvent
import org.eclipse.core.resources.IResourceChangeListener
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jface.viewers.LabelProviderChangedEvent
import org.eclipse.ui.PlatformUI

class ResourceDecoratorPostChangeListener implements IResourceChangeListener {

	override resourceChanged(IResourceChangeEvent event) {
		val manager = PlatformUI.workbench.decoratorManager
		val decorator = manager.getBaseLabelProvider(ResourceDecorator.canonicalName) as ResourceDecorator

		if (decorator !== null) {
			val affectedResources = newLinkedList
			event.delta.accept[affectedResources.add(resource)] // POST_CHANGE event produces delta (only) => collect those resources
			// additionally add classpath entry objects for source folders
			val affectedProjects = affectedResources.map[project].filterNull.toSet
			val affectedClassPaths = affectedProjects.map[sourceCodeClasspathEntries].flatten

			val labelEvent = new LabelProviderChangedEvent(decorator,
				(affectedClassPaths + affectedResources).toList.toArray)
			decorator.fireLabelEvent(labelEvent) // fire one event for all affected
		}
	}

	private def Iterable<IClasspathEntry> getSourceCodeClasspathEntries(IProject project) {
		if (JavaProject.hasJavaNature(project)) {
			val javaProject = JavaCore.create(project)
			return javaProject.readRawClasspath.filter[entryKind == IClasspathEntry.CPE_SOURCE]
		}
		return emptyList
	}

}
