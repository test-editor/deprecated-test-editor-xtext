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
import org.eclipse.e4.ui.model.application.ui.basic.MPartStack
import org.eclipse.e4.ui.model.application.ui.basic.MWindow
import org.eclipse.e4.ui.workbench.IPresentationEngine
import org.eclipse.e4.ui.workbench.modeling.EModelService
import org.eclipse.e4.ui.workbench.modeling.EPartService
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.slf4j.LoggerFactory

import static org.testeditor.rcp4.Constants.*
import org.testeditor.dsl.common.ui.workbench.MUIElementUtils

class ResetUIHandler {
	
	extension MUIElementUtils muiUtils=new MUIElementUtils  

	static val logger = LoggerFactory.getLogger(ResetUIHandler)
	@Inject MApplication application

	@Execute
	public def void resetUI(EModelService modelService, EPartService partService) {
		val window = application.children.head
		val perspectiveStack = modelService.find(MAIN_PERSPECTIVE_STACK_ID, application) as MPerspectiveStack
		val originalPerspective = modelService.cloneSnippet(application, PERSPECTIVE_ID, window) as MPerspective
		if (originalPerspective === null) {
			logger.warn('Could not reset UI since no original perspective has been saved (see SaveUIHandler.saveUI()).')
			return
		}

		val minimizedPartStacks = originalPerspective.flattenTree.filter(MPartStack).filter [
			tags.contains(IPresentationEngine.MINIMIZED)
		]
		if (perspectiveStack.selectedElement !== null) {
			modelService.removePerspectiveModel(perspectiveStack.selectedElement, window)
		}
		perspectiveStack.children.add(originalPerspective)
		minimizedPartStacks.forEach[tags.remove(IPresentationEngine.MINIMIZED)]
		originalPerspective.flattenTree.forEach[visible = true]
		modelService.resetPerspectiveModel(originalPerspective, window)
		try {
			window.ensureActiveFor[partService.switchPerspective(originalPerspective)]
			minimizedPartStacks.forEach[tags.add(IPresentationEngine.MINIMIZED)]
			logger.info('UI was reset.')
		} catch (Exception e) {
			logger.error('UI was not correctly reset. Exception during switch of perspective.', e)
		}
	}

	private def void ensureActiveFor(MWindow window, (Object)=>void action) {
		Display.^default.syncExec(new Runnable {
			override run() {
				(window.widget as Shell).forceFocus
				action.apply(window)
			}
		});
	}

}
