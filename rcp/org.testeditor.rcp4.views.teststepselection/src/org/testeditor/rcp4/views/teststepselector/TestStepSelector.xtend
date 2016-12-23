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

import java.util.List
import java.util.Map
import java.util.Set
import javax.annotation.PostConstruct
import javax.annotation.PreDestroy
import javax.inject.Inject
import org.eclipse.core.runtime.jobs.IJobChangeEvent
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.jobs.JobChangeAdapter
import org.eclipse.e4.core.di.annotations.Optional
import org.eclipse.e4.core.di.extensions.EventTopic
import org.eclipse.e4.core.services.events.IEventBroker
import org.eclipse.e4.ui.di.Focus
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.jface.viewers.TreeViewer
import org.eclipse.swt.SWT
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.commands.ICommandService
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.IContainer
import org.eclipse.xtext.resource.IResourceDescription
import org.eclipse.xtext.resource.IResourceDescriptions
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.resource.containers.StateBasedContainer
import org.eclipse.xtext.resource.impl.ResourceDescriptionsProvider
import org.eclipse.xtext.ui.editor.XtextEditor
import org.slf4j.LoggerFactory
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.naming.AmlQualifiedNameProvider

import static org.testeditor.aml.AmlPackage.Literals.AML_MODEL
import static org.testeditor.rcp4.views.teststepselector.XtendSWTLib.*

/** 
 * part that display a tree view with drag and drop elements of the aml model which can be inserted into
 * a tcl document
 */
class TestStepSelector {

	public static val String SELECTOR_UPDATE_VIEW = "MaskStepSelector_Update_View"

	static val logger = LoggerFactory.getLogger(TestStepSelector)

	@Inject IEventBroker broker
	@Inject AmlInjectorProvider amlInjectorProvider
	@Inject TestStepSelectorLabelProvider labelProvider
	@Inject ICommandService commandService

	AmlQualifiedNameProvider amlQualifiedNameProvider
	IContainer.Manager containerManager
	ResourceDescriptionsProvider resourceDescriptionsProvider
	IResourceDescription.Manager resourcenDescriptionManger
	String currentProject
	TreeViewer viewer

	Map<String, Set<String>> expandedElementsPerProject = newHashMap
	
	val triggerViewUpdate = buildingWorkpaceCompleted [
			displaySyncExec [
				val workbenchWindow = PlatformUI.workbench.activeWorkbenchWindow
				if (workbenchWindow !== null) {
					logger.info("Trigger update TestStepSelector.")
					broker.post(TestStepSelector.SELECTOR_UPDATE_VIEW, workbenchWindow.activePage?.activeEditor)
				}
			]
		]

	@PostConstruct
	def void postConstruct(Composite parent, TestStepSelectorExecutionListener executionListener,
		TestStepSelectorPartListener partListener, TestStepSelectorDragSourceListener dragSourceListener) {

		val amlInjector = amlInjectorProvider.get
		amlQualifiedNameProvider = amlInjector.getInstance(AmlQualifiedNameProvider)
		containerManager = amlInjector.getInstance(IContainer.Manager)
		resourcenDescriptionManger = amlInjector.getInstance(IResourceDescription.Manager)
		resourceDescriptionsProvider = amlInjector.getInstance(ResourceDescriptionsProvider)

		commandService.addExecutionListener(executionListener)
		val activePage = PlatformUI.workbench.activeWorkbenchWindow.activePage
		activePage.addPartListener(partListener)

		viewer = newTreeViewer(parent, SWT.V_SCROLL) [
			addDragSupport((DND.DROP_COPY.bitwiseOr(DND.DROP_MOVE)), #[TextTransfer.instance], dragSourceListener)
		]
		dragSourceListener.viewer = viewer
		viewer.contentProvider = amlInjectorProvider.get.getInstance(TestStepSelectorTreeContentProvider)
		viewer.labelProvider = labelProvider
		Job.jobManager.addJobChangeListener(triggerViewUpdate)
	}
	
	@PreDestroy
	def void preDestroy() {
		Job.jobManager.removeJobChangeListener(triggerViewUpdate)
	}

	/**
	 * register code to be run after workspace build job has completed
	 */
	def private JobChangeAdapter buildingWorkpaceCompleted(()=>void closure) {
		return new JobChangeAdapter {
			override done(IJobChangeEvent event) {
				if (event.job.name.equals("Building workspace")) {
					logger.info("Building workspace completed.")
					closure.apply
				}
			}
		}
	}

	/**
	 * run code within ui thread, only if display has not been disposed yet
	 */
	def private void displaySyncExec(()=>void closure) {
		Display.^default => [
			if (!isDisposed) {
				syncExec(closure)
			}
		]
	}

	@Focus
	def void setFocus() {
		viewer.control.setFocus
	}

	@Inject
	@Optional
	def void updateView(@EventTopic(SELECTOR_UPDATE_VIEW) Object data) {
		logger.debug("updateView for data='{}'", data)
		if (data instanceof XtextEditor) {
			Display.^default.syncExec[updateViewForXtextEditor(data)]
		}
	}

	private def updateViewForXtextEditor(XtextEditor editor) {
		val projectName = editor.resource.project.name

		var previouslyExpandedElements = viewer.expandedElements.map[toStringPath].toSet

		viewer.input = editor.document.readOnly[getVisibleAMLModels]
		if (currentProject === null) {
			currentProject = projectName
		}
		if (currentProject != projectName) {
			expandedElementsPerProject.put(currentProject, previouslyExpandedElements)
			previouslyExpandedElements = expandedElementsPerProject.get(projectName)
			currentProject = projectName
		}
		if (previouslyExpandedElements !== null && !previouslyExpandedElements.empty) {
			viewer.expandedElements = elementsToExpand(previouslyExpandedElements, viewer.input as Iterable<AmlModel>)
		}
	}

	private def List<AmlModel> getVisibleAMLModels(XtextResource resource) {
		val currentModels = newArrayList

		val resourceSet = amlInjectorProvider.get.getInstance(ResourceSet)
		val resourceDescription = resourcenDescriptionManger.getResourceDescription(resource)
		val IResourceDescriptions resourceDescriptions = resourceDescriptionsProvider.createResourceDescriptions

		val visibleContainers = containerManager.getVisibleContainers(resourceDescription, resourceDescriptions)

		visibleContainers.filter(StateBasedContainer).forEach [
			currentModels += it.getAmlModels(resourceSet)
		]

		return currentModels
	}

	private def List<AmlModel> getAmlModels(StateBasedContainer container, ResourceSet resourceSet) {
		val amlDescriptions = container.getExportedObjectsByType(AML_MODEL)

		amlDescriptions.map[EObjectOrProxy].map [
			EcoreUtil2.resolve(it, resourceSet) as AmlModel
		].toList
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
			String:
				return object
			EObject:
				return amlQualifiedNameProvider.apply(object).toString
			default:
				throw new IllegalArgumentException("unexpected type " + object.class.name + " in expanded TreeElements")
		}
	}

}
