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

import java.util.function.Function
import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Execute
import org.eclipse.e4.ui.model.application.MApplication
import org.eclipse.e4.ui.model.application.ui.MElementContainer
import org.eclipse.e4.ui.model.application.ui.MUIElement
import org.eclipse.e4.ui.model.application.ui.advanced.MPerspective
import org.eclipse.e4.ui.model.application.ui.advanced.MPerspectiveStack
import org.eclipse.e4.ui.model.application.ui.basic.MPartStack
import org.eclipse.e4.ui.model.application.ui.basic.MWindow
import org.eclipse.e4.ui.workbench.IPresentationEngine
import org.eclipse.e4.ui.workbench.modeling.EModelService
import org.eclipse.e4.ui.workbench.modeling.EPartService
import org.slf4j.LoggerFactory
import org.testeditor.rcp4.Constants

class ResetUIHandler {

	static val logger = LoggerFactory.getLogger(ResetUIHandler)
	@Inject MApplication application

	@Execute
	public def void resetUI(EModelService modelService, EPartService partService) {
		val window = application.getChildren().get(0) as MWindow
		val perspectiveStack = modelService.find(Constants.MAIN_PERSPECTIVE_STACK_ID, application) as MPerspectiveStack
		val originalPerspective = modelService.cloneSnippet(application, Constants.PERSPECTIVE_ID, window) as MPerspective
		if (originalPerspective === null) {
			logger.warn('Could not reset UI since no original perspective has been saved (see SaveUIHandler.saveUI()).')
			return
		}

		if (perspectiveStack.selectedElement !== null) {
			modelService.removePerspectiveModel(perspectiveStack.selectedElement, window)
		}
		perspectiveStack.children.add(originalPerspective)
		val minimizedPartStacks = originalPerspective.recursivelyGetContainedWhere(MPartStack) [
			tags.contains(IPresentationEngine.MINIMIZED)
		]
		minimizedPartStacks.forEach[tags.remove(IPresentationEngine.MINIMIZED)]
		originalPerspective.recursiveVisible = true
		modelService.resetPerspectiveModel(originalPerspective, window)
		try {
			partService.switchPerspective(originalPerspective)
		} catch (Exception e) {
			logger.error('Exception during switch of perspective.', e)
		}
		minimizedPartStacks.forEach[tags.add(IPresentationEngine.MINIMIZED)]
		logger.info('UI was reset.')
	}

	/** get the recursively contained elements of this ui element of the given clazz for which the predicate holds true */
	private def <T> Iterable<T> recursivelyGetContainedWhere(MUIElement element, Class<T> clazz,
		Function<? super T, Boolean> predicate) {
		val result = newLinkedList

		if (element instanceof MElementContainer<?>) {
			result += element.children.map[recursivelyGetContainedWhere(clazz, predicate)].flatten
		}

		if (clazz.isInstance(element) && (predicate.apply(element as T))) {
			result.add(element as T)
		}

		return result
	}

	/** recursively set the visibility of all ui elements */
	private def void setRecursiveVisible(MUIElement element, boolean visible) {
		element.visible = visible
		if (element instanceof MElementContainer<?>) {
			element.children.forEach[recursiveVisible = visible]
		}
	}

}
