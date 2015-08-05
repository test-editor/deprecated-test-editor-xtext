/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
package org.testeditor.aml.dsl.ui.wizard

import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor

/**
 * Keeps the project clean for now.
 */
class CustomizedAmlProjectCreator extends AmlProjectCreator {

	override protected enhanceProject(IProject project, IProgressMonitor monitor) throws CoreException {
		// do nothing
	}

	override protected String[] getProjectNatures() {
		return super.projectNatures.filter[it != "org.eclipse.pde.PluginNature"]
	}
	
	override protected getBuilders() {
		return super.builders.filter[!it.startsWith("org.eclipse.pde")]
	}

}