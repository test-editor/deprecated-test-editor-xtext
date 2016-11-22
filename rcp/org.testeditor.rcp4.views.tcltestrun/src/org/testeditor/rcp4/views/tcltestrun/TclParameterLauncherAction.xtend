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

import org.testeditor.dsl.common.util.PlatformHelper
import javax.inject.Inject

class TclParameterLauncherAction extends TclLauncherAction {
	
	@Inject 
	protected new(PlatformHelper platformHelper) {
		super(platformHelper)
	}
	
	override protected withParameter() {
		return true
	}
	
}
