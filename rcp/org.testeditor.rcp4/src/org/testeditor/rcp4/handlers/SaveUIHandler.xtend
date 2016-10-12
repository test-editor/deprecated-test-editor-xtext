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
import org.slf4j.LoggerFactory

import static org.testeditor.rcp4.Constants.*

class SaveUIHandler {

	static val logger = LoggerFactory.getLogger(ResetUIHandler)

	@Inject MApplication application

	@Execute
	public def void saveUI(EModelService modelService) {
		val perspectiveStack = modelService.find(MAIN_PERSPECTIVE_STACK_ID, application) as MPerspectiveStack
		if (perspectiveStack !== null) {
			try {
				modelService.cloneElement(perspectiveStack.selectedElement, application) as MPerspective
				logger.info("Saved current UI (perspective) for reset.")
			} catch (Exception e) {
				logger.warn("Unable to save current UI (perspective) for reset. Exception during save.", e)
			}
		} else {
			logger.warn(
				"Unable to save current UI (perspective) for reset. Perspective ID='{}' not found by model service.",
				MAIN_PERSPECTIVE_STACK_ID)
			}
		}

	}
	