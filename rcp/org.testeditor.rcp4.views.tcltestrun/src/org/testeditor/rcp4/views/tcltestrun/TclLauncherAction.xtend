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
package org.testeditor.rcp4.views.tcltestrun

import com.google.inject.Inject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.FileLocator
import org.eclipse.core.runtime.Path
import org.eclipse.jface.action.Action
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.jface.viewers.StructuredSelection
import org.eclipse.ui.ISelectionService
import org.testeditor.dsl.common.ui.workbench.PartHelper
import org.testeditor.dsl.common.util.PlatformHelper

class TclLauncherAction extends Action {

	@Inject TclLauncherUi launcherUI
	@Inject PartHelper partHelper

	@Inject
	protected new(PlatformHelper platformHelper) {
		val bundle = platformHelper.getBundle("org.testeditor.rcp4.views.tcltestrun")
		val path = new Path("icons/run_test.png")
		val url = FileLocator.find(bundle, path, null)
		imageDescriptor = ImageDescriptor.createFromURL(url)
	}

	override run() {
		val selection = partHelper.findPart("org.testeditor.rcp4.views.ProjectExplorer").viewSite.getService(
			ISelectionService).selection
		if (!selection.empty) {
			val sel = selection as StructuredSelection
			val firstElement = sel.firstElement
			if (firstElement instanceof IResource) {
				launcherUI.launch(sel, firstElement.project, null, withParameter)
			}
		}
	}

	def protected boolean withParameter() {
		return false
	}

}
