package org.testeditor.rcp4.views.projectexplorer

import javax.inject.Inject
import org.eclipse.core.resources.IResourceChangeEvent
import org.eclipse.core.resources.IResourceChangeListener
import org.eclipse.jface.viewers.LabelProviderChangedEvent
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.ui.utils.WorkbenchHelper
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

class ResourceDecoratorPostChangeListener implements IResourceChangeListener {

	static val logger = LoggerFactory.getLogger(ResourceDecoratorPostChangeListener)

	@Inject ClasspathUtil classpathUtil
	@Inject WorkbenchHelper workbenchHelper

	override resourceChanged(IResourceChangeEvent event) {
		val manager = workbenchHelper.decoratorManager
		val decorator = manager.getBaseLabelProvider(ResourceDecorator.canonicalName) as ResourceDecorator

		if (decorator !== null) {
			val affectedResources = newLinkedList
			event.delta.accept[affectedResources.add(resource)] // POST_CHANGE event produces delta (only) => collect those resources
			// additionally add classpath entry objects for source folders, these may contain generated artefacts
			val affectedProjects = affectedResources.map[project].filterNull.toSet
			val affectedClassPaths = affectedProjects.map[classpathUtil.getSourceClasspathEntries(it)].flatten

			val labelEvent = new LabelProviderChangedEvent(decorator,
				(affectedClassPaths + affectedResources).toList.toArray)
			decorator.fireLabelEvent(labelEvent) // fire one event for all affected
		} else {
			logger.
				warn('''No (active) decorator found for id='«ResourceDecorator.canonicalName»'. Project(-explorer) may seem ok despite of errors.''')
		}
	}

}
