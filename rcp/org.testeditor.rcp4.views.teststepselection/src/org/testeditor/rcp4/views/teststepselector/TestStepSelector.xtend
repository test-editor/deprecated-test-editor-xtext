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
package org.testeditor.rcp4.views.teststepselector

import java.util.Set
import javax.annotation.PostConstruct
import javax.inject.Inject
import org.eclipse.core.runtime.jobs.IJobChangeEvent
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.jobs.JobChangeAdapter
import org.eclipse.e4.core.di.annotations.Optional
import org.eclipse.e4.core.di.extensions.EventTopic
import org.eclipse.e4.core.services.events.IEventBroker
import org.eclipse.e4.ui.di.Focus
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.viewers.TreeViewer
import org.eclipse.swt.SWT
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.commands.ICommandService
import org.slf4j.LoggerFactory
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.naming.AmlQualifiedNameProvider

import static org.testeditor.rcp4.views.teststepselector.XtendSWTLib.*

/** 
 * part that display a tree view with drag and drop elements of the aml model which can be inserted into
 * a tcl document
 */
class TestStepSelector {

	public static val String SELECTOR_UPDATE_MODEL_AND_VIEW = "MaskStepSelector_Update_Model"
	public static val String SELECTOR_UPDATE_VIEW = "MaskStepSelector_Update_View"

	static val logger = LoggerFactory.getLogger(TestStepSelector)

	@Inject IEventBroker broker
	@Inject AmlInjectorProvider amlInjectorProvider
	@Inject TestStepSelectorLabelProvider labelProvider
	@Inject ICommandService commandService
	TestStepSelectorTreeContentProvider contentProvider
	AmlQualifiedNameProvider amlQualifiedNameProvider
	TreeViewer viewer

	Iterable<AmlModel> models

	@PostConstruct
	def void postConstruct(Composite parent, TestStepSelectorExecutionListener executionListener,
		TestStepSelectorPartListener partListener, TestStepSelectorDragSourceListener dragSourceListener) {

		val amlInjector = amlInjectorProvider.get
		amlQualifiedNameProvider = amlInjector.getInstance(AmlQualifiedNameProvider)
		commandService.addExecutionListener(executionListener)
		val page = PlatformUI.workbench.activeWorkbenchWindow.activePage
		page.addPartListener(partListener)

		viewer = newTreeViewer(parent, SWT.V_SCROLL) [
			addDragSupport((DND.DROP_COPY.bitwiseOr(DND.DROP_MOVE)), #[TextTransfer.instance], dragSourceListener)
		]
		dragSourceListener.viewer = viewer
		viewer.contentProvider = amlInjectorProvider.get.getInstance(TestStepSelectorTreeContentProvider)
		viewer.labelProvider = labelProvider
		
		Job.jobManager.addJobChangeListener(new JobChangeAdapter {
			override done(IJobChangeEvent event) {
				if (event.job.name.equals("Building workspace")) {
					logger.info("Building workspace completed. Trigger update TestStepSelector")
					broker.post(TestStepSelector.SELECTOR_UPDATE_MODEL_AND_VIEW, null)
				}
			}
		})

	}

	@Focus
	def void setFocus() {
		viewer.control.setFocus
	}

	@Inject
	@Optional
	def void updateModelAndView(@EventTopic(SELECTOR_UPDATE_MODEL_AND_VIEW) Object data) {
		logger.debug("updateModelAndView")
		val amlInjector = amlInjectorProvider.get
		models = amlInjector.getInstance(AmlModelsProvider).amlModels
		Display.getDefault.syncExec[internalUpdateView]
	}

	@Inject
	@Optional
	def void updateView(@EventTopic(SELECTOR_UPDATE_VIEW) Object data) {
		logger.debug("updateView")
		// TODO - check on the project of the editor
		if (viewer.input === null) {
			Display.^default.syncExec[internalUpdateView]
		}
	}

	private def internalUpdateView() {
		// Initial call to the selector
		if (viewer.input == null) {
			viewer.input = models
		} else {
			// update the view model and conserve the expansion
			val previouslyExpandedElements = viewer.expandedElements.map[toStringPath].toSet
			viewer.input = models
			if(!previouslyExpandedElements.empty){
				viewer.expandedElements = elementsToExpand(previouslyExpandedElements, models)
			}
		}
	}
	
	private def Object[] elementsToExpand(Set<String> previouslyExpandedElements, Iterable<AmlModel> model) {
		val elementsToExpand = newArrayList
		model.forEach [
			val pathString = toStringPath
			if (previouslyExpandedElements.contains(pathString)) {
				elementsToExpand.add(pathString) // since AmlModels are added by string (not EObject)
			}
			components.forEach [
				if (previouslyExpandedElements.contains(toStringPath)) {
					elementsToExpand.add(it)
				}
				elements.forEach [
					if (previouslyExpandedElements.contains(toStringPath)) {
						elementsToExpand.add(it)
					}
				]
			]
		]
		return elementsToExpand
	}

	def private String toStringPath(Object object) {
		switch (object) {
			String: return object
			EObject: return amlQualifiedNameProvider.apply(object).toString
			default: throw new IllegalArgumentException("unexpected type " + object.class.name +
				" in expanded TreeElements")
		}
	}

}
