/** 
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 */
package org.testeditor.rcp4.views.projectexplorer

import org.eclipse.core.resources.IResourceChangeEvent
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.jface.viewers.LabelProviderChangedEvent
import org.eclipse.swt.widgets.Composite
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.navigator.CommonNavigator

class ProjectExplorer extends CommonNavigator {

	override void createPartControl(Composite parent) {
		super.createPartControl(parent)
		// TODO Make this configurable with a new Configuration dialog.
		setLinkingEnabled(true)

		ResourcesPlugin.workspace.addResourceChangeListener([resourcePostChangeSubscription],
			IResourceChangeEvent.POST_CHANGE);
	}

	def void resourcePostChangeSubscription(IResourceChangeEvent data) {
		val manager = PlatformUI.workbench.decoratorManager
		val decorator = manager.getBaseLabelProvider(ResourceDecorator.canonicalName) as ResourceDecorator

		if (decorator !== null) {
			val affectedResources = newLinkedList
			data.delta.accept[affectedResources.add(resource)] // POST_CHANGE event produces delta => collect those resources
			val event = new LabelProviderChangedEvent(decorator, affectedResources.toArray)
			decorator.fireLabelEvent(event) // fire one event for all affected resources
		}
	}

}
