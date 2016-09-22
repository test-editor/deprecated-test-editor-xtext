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
package org.testeditor.rcp4.handlers

import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Execute
import org.eclipse.e4.ui.model.application.MApplication
import org.eclipse.e4.ui.model.application.ui.advanced.MPerspective
import org.eclipse.e4.ui.model.application.ui.advanced.MPerspectiveStack
import org.eclipse.e4.ui.workbench.modeling.EModelService
import org.testeditor.rcp4.Constants

class SaveUIHandler {

	@Inject MApplication application
	
	@Execute
	public def void saveUI(EModelService modelService) {
		val perspectiveStack = modelService.find(Constants.MAIN_PERSPECTIVE_STACK_ID, application) as MPerspectiveStack
		modelService.cloneElement(perspectiveStack.selectedElement, application) as MPerspective
	}

}
