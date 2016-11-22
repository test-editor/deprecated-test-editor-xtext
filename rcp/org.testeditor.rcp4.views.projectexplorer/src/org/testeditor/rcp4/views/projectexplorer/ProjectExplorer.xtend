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

import javax.inject.Inject
import org.eclipse.core.resources.IResourceChangeEvent
import org.eclipse.swt.widgets.Composite
import org.eclipse.ui.navigator.CommonNavigator
import org.testeditor.dsl.common.util.WorkspaceHelper

class ProjectExplorer extends CommonNavigator {

	@Inject WorkspaceHelper workspaceHelper
	@Inject ResourceDecoratorPostChangeListener changeListener

	override void createPartControl(Composite parent) {
		super.createPartControl(parent)
		// TODO Make this configurable with a new Configuration dialog.
		setLinkingEnabled(true)

		workspaceHelper.addResourceChangeListener(changeListener,
			IResourceChangeEvent.POST_CHANGE);
	}
	
	override dispose() {
		workspaceHelper.removeResourceChangeListener(changeListener)
	}

}
