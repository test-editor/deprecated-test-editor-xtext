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
package org.testeditor.rcp4.views

import org.eclipse.core.runtime.FileLocator
import org.eclipse.core.runtime.Path
import org.eclipse.e4.core.di.annotations.Creatable
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.swt.graphics.Image
import org.osgi.framework.FrameworkUtil
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType

/** provide icons for the drag/drop tree view for tcl - editor insertions */
@Creatable
class AmlModelIconProvider {
	private static val BUNDLE = FrameworkUtil.getBundle(AmlModelIconProvider);

	private static val ICON_FOLDER = ImageDescriptor.createFromURL(
		FileLocator.find(BUNDLE, new Path("icons/folder.png"), null)).createImage
	private static val ICON_MASK = ImageDescriptor.createFromURL(
		FileLocator.find(BUNDLE, new Path("icons/mask.png"), null)).createImage
	private static val ICON_STEP = ImageDescriptor.createFromURL(
		FileLocator.find(BUNDLE, new Path("icons/text_list_bullets.png"), null)).createImage
	private static val ICON_ELEMENT = ImageDescriptor.createFromURL(
		FileLocator.find(BUNDLE, new Path("icons/brick.png"), null)).createImage

	def dispatch Image getIcon(EObject element) {
		null
	}

	def dispatch Image getIcon(AmlModel element) {
		ICON_FOLDER
	}

	def dispatch Image getIcon(Component element) {
		ICON_MASK
	}

	def dispatch Image getIcon(ComponentElement element) {
		ICON_ELEMENT
	}

	def dispatch Image getIcon(InteractionType element) {
		ICON_STEP
	}
}
