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

import java.util.ArrayList
import java.util.Set
import javax.annotation.PostConstruct
import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Optional
import org.eclipse.e4.core.di.extensions.EventTopic
import org.eclipse.e4.ui.di.Focus
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.viewers.TreeViewer
import org.eclipse.swt.SWT
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.DragSourceEvent
import org.eclipse.swt.dnd.DragSourceListener
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.commands.ICommandService
import org.slf4j.LoggerFactory
import org.testeditor.aml.AmlModel

import static org.testeditor.rcp4.views.teststepselector.XtendSWTLib.*
import org.testeditor.aml.dsl.naming.AmlQualifiedNameProvider
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.jobs.JobChangeAdapter
import org.eclipse.e4.core.services.events.IEventBroker
import org.eclipse.core.runtime.jobs.IJobChangeEvent

/** 
 * part that display a tree view with drag and drop elements of the aml model which can be inserted into
 * a tcl document
 */
class TestStepSelector {

	public static val String SELECTOR_UPDATE_MODEL_AND_VIEW = "MaskStepSelector_Update_Model"
	public static val String SELECTOR_UPDATE_VIEW = "MaskStepSelector_Update_View"

	static val logger = LoggerFactory.getLogger(TestStepSelector);

	@Inject IEventBroker broker
	@Inject AmlInjectorProvider amlInjectorProvider
	@Inject TestStepSelectorLabelProvider labelProvider
	@Inject AmlDropSupport amlDropSupport
	@Inject TestStepSelectorDropTextProvider dropTextProvider
	@Inject ICommandService commandService
	TestStepSelectorTreeContentProvider contentProvider;
	AmlQualifiedNameProvider amlQualifiedNameProvider
	TreeViewer viewer
	
	Iterable<AmlModel> model
	
	@PostConstruct
	def void postConstruct(Composite parent, TestStepSelectorExecutionListener executionListener,TestStepSelectorPartListener partListener) {
		
		val amlInjector = amlInjectorProvider.get
		amlQualifiedNameProvider = amlInjector.getInstance(AmlQualifiedNameProvider)
		contentProvider = amlInjectorProvider.get.getInstance(TestStepSelectorTreeContentProvider)
		commandService.addExecutionListener(executionListener)
		val page = PlatformUI.workbench.activeWorkbenchWindow.activePage;
		page.addPartListener(partListener)

		viewer = newTreeViewer(parent, SWT.V_SCROLL) [
			addDragSupport((DND.DROP_COPY.bitwiseOr(DND.DROP_MOVE)), #[TextTransfer.instance],
				new DragSourceListener {

					override dragFinished(DragSourceEvent event) {
						logger.trace("drag stop")
					}

					override dragSetData(DragSourceEvent event) {
						if (TextTransfer.instance.isSupportedType(event.dataType) &&
							amlDropSupport.dropSupported(viewer.structuredSelection.firstElement as EObject)) {
							event.data = dropTextProvider.getText(viewer.structuredSelection)
						} else {
							event.data = ""
						}
					}

					override dragStart(DragSourceEvent event) {
						logger.trace("drag start")
					}

				})
		]
		
		Job.getJobManager().addJobChangeListener(new JobChangeAdapter() {
			override done(IJobChangeEvent event) {
				if (event.job.name.equals("Building workspace")) {
					logger.info("Building workspace completed. Trigger update TestSetSelector")
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
		updateModel
		Display.getDefault.syncExec[internalUpdateView()]
	}

	@Inject
	@Optional
	def void updateView(@EventTopic(SELECTOR_UPDATE_VIEW) Object data) {
		logger.debug("updateView")
		// TODO - check on the project of the editor
		if(viewer.input == null){
			Display.getDefault.syncExec[internalUpdateView()]
		}
	}

	private def internalUpdateView() {

		//Initial call to the selector
		if (viewer.input == null) {
			viewer.labelProvider = labelProvider
			viewer.contentProvider = contentProvider
			viewer.input = model
		} else {
			// update the view model and conserver the selection
			val Set<String> expandedElements = viewer.expandedElements.map[toStringPath(it)].toSet
			viewer.input = model;
			viewer.expandedElements = expandElements(expandedElements, model)
		}

	}

	private def void updateModel() {
		val amlInjector = amlInjectorProvider.get
		model = amlInjector.getInstance(AmlModelsProvider).amlModels
	}

	private def Object[] expandElements(Set<String> elements, Iterable<AmlModel> model) {
		val elementsToExpand = new ArrayList<Object>();
		model.forEach [
			{
				if (elements.contains(it.toStringPath)) {
					elementsToExpand.add(it.toStringPath)
				}
				it.components.forEach [
					if (elements.contains(it.toStringPath)) {
						elementsToExpand.add(it)
					}
					it.elements.forEach [
						if (elements.contains(it.toStringPath)) {
							elementsToExpand.add(it)
						}
					]
				]
			}
		]
		return elementsToExpand
	}

	def private String toStringPath(Object object) {
		switch (object) {
			String:
				return object
			EObject:
				return amlQualifiedNameProvider.apply(object).toString()
			default:
				throw new IllegalArgumentException("unexpected type " + object.class.name + " in expanded TreeElements")
		}
	}


}
