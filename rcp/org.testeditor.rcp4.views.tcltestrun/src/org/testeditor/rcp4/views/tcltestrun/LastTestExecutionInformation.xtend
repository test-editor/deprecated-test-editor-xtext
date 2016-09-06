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

import java.util.List
import java.util.Map
import org.eclipse.core.resources.IProject
import org.eclipse.xtend.lib.annotations.Accessors
import org.testeditor.rcp4.tcltestrun.TclLauncher

class LastTestExecutionInformation {

	@Accessors
	List<String> testCasesCommaList
	@Accessors
	IProject project
	@Accessors
	TclLauncher launcher
	@Accessors
	Map<String, Object> options

}
