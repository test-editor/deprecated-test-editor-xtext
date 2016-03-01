/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.rcp4.views.teststepselector

import com.google.common.cache.CacheBuilder
import com.google.common.cache.LoadingCache
import javax.annotation.PreDestroy
import org.eclipse.core.runtime.FileLocator
import org.eclipse.core.runtime.Path
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.swt.graphics.Image
import org.osgi.framework.FrameworkUtil
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType

// TODO extract some stuff in an abstract super class
/** provide icons for the drag/drop tree view for tcl - editor insertions */
@Creatable
class AmlModelIconProvider {

	private static val BUNDLE = FrameworkUtil.getBundle(AmlModelIconProvider)

	// TODO check if we can use default Eclipse icons here
	private static val namespace = "namespace.gif".descriptor
	private static val mask = "mask.png".descriptor
	private static val interaction = "interaction.gif".descriptor
	private static val componentElement = "component_element.png".descriptor

	static private def ImageDescriptor getDescriptor(String iconName) {
		return ImageDescriptor.createFromURL(FileLocator.find(BUNDLE, new Path("icons/" + iconName), null))
	}

	val LoadingCache<ImageDescriptor, Image> cache = CacheBuilder.newBuilder.removalListener [ notification |
		val Image image = notification.value
		image.dispose
	].build [
		createImage
	]

	def dispatch Image getIcon(Object element) {
		null
	}

	/** If we pass a String, we expect this to be the namespace. */
	def dispatch Image getIcon(String element) {
		cache.get(namespace)
	}

	def dispatch Image getIcon(Component element) {
		cache.get(mask)
	}

	def dispatch Image getIcon(ComponentElement element) {
		cache.get(componentElement)
	}

	def dispatch Image getIcon(InteractionType element) {
		cache.get(interaction)
	}

	// TODO check when and if this is ever called
	@PreDestroy
	def void dispose() {
		cache.invalidateAll
	}

}
