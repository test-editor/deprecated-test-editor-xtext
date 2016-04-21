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
package org.testeditor.rcp4

import org.eclipse.jface.action.IMenuManager
import org.eclipse.ui.IWorkbenchWindow
import org.eclipse.ui.application.ActionBarAdvisor
import org.eclipse.ui.application.IActionBarConfigurer

/** dummy class */
class ApplicationActionBarAdvisor extends ActionBarAdvisor {
	new(IActionBarConfigurer configurer) {
		super(configurer)
	}

	override makeActions(IWorkbenchWindow window) {
	}

	override fillMenuBar(IMenuManager menuBar) {
	}
}
