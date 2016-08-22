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
import java.util.Iterator
import java.util.Set
import java.util.TreeSet
import javax.annotation.PostConstruct
import javax.annotation.PreDestroy
import javax.inject.Inject
import org.eclipse.core.commands.ExecutionEvent
import org.eclipse.core.commands.ExecutionException
import org.eclipse.core.commands.IExecutionListener
import org.eclipse.core.commands.NotHandledException
import org.eclipse.e4.core.di.annotations.Optional
import org.eclipse.e4.core.di.extensions.EventTopic
import org.eclipse.e4.core.services.events.IEventBroker
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
import org.eclipse.ui.IPartListener
import org.eclipse.ui.IWorkbenchCommandConstants
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.commands.ICommandService
import org.eclipse.ui.texteditor.ITextEditor
import org.slf4j.LoggerFactory
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.dsl.ui.internal.DslActivator

import static org.testeditor.rcp4.views.teststepselector.XtendSWTLib.*

/** 
 * part that display a tree view with drag and drop elements of the aml model which can be inserted into
 * a tcl document
 */
class TestStepSelector {

	public static val String SELECTOR_TOPIC_UPDATE = "MaskStepSelector_Update"
	public static val String SELECTOR_TOPIC_REFRESH = "MaskStepSelector_Refresh"

	static val logger = LoggerFactory.getLogger(TestStepSelector);

	@Inject IEventBroker broker
	@Inject AmlInjectorProvider amlInjectorProvider
	@Inject TestStepSelectorLabelProvider labelProvider
	@Inject TestStepSelectorDropTextProvider dropTextProvider
	@Inject AmlDropSupport amlDropSupport

	/** set as soon as the view is populated */
	TreeViewer viewer

	@PostConstruct
	def void postConstruct(Composite parent) {
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
		val commandService = PlatformUI.workbench.getService(ICommandService)
		commandService.addExecutionListener(new IExecutionListener() {

			override notHandled(String arg0, NotHandledException arg1) {
			}

			override postExecuteFailure(String arg0, ExecutionException arg1) {
			}

			override postExecuteSuccess(String commandName, Object arg1) {
				if (IWorkbenchCommandConstants.FILE_SAVE.equals(commandName)) {
					val activePage = PlatformUI.workbench.activeWorkbenchWindow.activePage
					val id = activePage.activeEditor.editorSite.id
					if (DslActivator.ORG_TESTEDITOR_AML_DSL_AML.equals(id)) {
						broker.post(TestStepSelector.SELECTOR_TOPIC_UPDATE, null)
					}
				}
			}

			override preExecute(String arg0, ExecutionEvent arg1) {
			}

		})
		// ICommandService commandService = PlatformUI.workbench.service(ICommandService);
		val page = PlatformUI.workbench.activeWorkbenchWindow.activePage;

		page.addPartListener(new IPartListener() {

			override partActivated(IWorkbenchPart part) {
				if (part instanceof ITextEditor) {
					refreshView(null)
				}
			}

			override partBroughtToTop(IWorkbenchPart part) {
			}

			override partClosed(IWorkbenchPart part) {
			}

			override partDeactivated(IWorkbenchPart part) {
			}

			override partOpened(IWorkbenchPart part) {
			}

		})

	}

//	@Inject
//	@Optional
//	def void startupComplete(@EventTopic(UIEvents.UILifeCycle.APP_STARTUP_COMPLETE) Object data) {
//		// startup complete ensures the index to be populated
//		Display.getDefault.syncExec[populateViewIfEmpty] // is only run, if view is created on startup
//	}
	@Focus
	def void setFocus() {
		viewer.control.setFocus
	}

	@Inject
	@Optional
	def void updateView(@EventTopic(SELECTOR_TOPIC_UPDATE) Object data) {
		logger.debug("Updating view.")
		Display.getDefault.syncExec[updateView(true)]
	}

	@Inject
	@Optional
	def void refreshView(@EventTopic(SELECTOR_TOPIC_REFRESH) Object data) {
		Display.getDefault.syncExec[updateView(false)]
	}

	// make sure that the index is populated before calling this method
	// and that it is run in the swt thread!
	def updateView(boolean update) {

		if (!update && viewer.input != null) {
			return
		}
		val amlInjector = amlInjectorProvider.get


		viewer.labelProvider = labelProvider
		viewer.contentProvider = amlInjector.getInstance(TestStepSelectorTreeContentProvider)

		val amlModelsProvider = amlInjector.getInstance(AmlModelsProvider)
		if (viewer.input == null) {
			
			viewer.input = amlModelsProvider.amlModels
			
		} else {
			val Set<String> elements = storeExpandedElements()

			val model = amlModelsProvider.amlModels
			viewer.input = model;
			
			viewer.expandedElements = searchExpandedElementsInModel(elements, model)
		}

	}
	private def Object[] searchExpandedElementsInModel(Set<String> elements, Iterable<AmlModel> model) {
	
			val elementsToExpand = new ArrayList<Object>();

			model.forEach [
				{
					if (elements.contains(it.package)) {
						elementsToExpand.add(it.package)
					}
					val package = it.package;
					it.components.forEach [
						val componentName = package + ">" + it.name;
						if (elements.contains(componentName)) {
							elementsToExpand.add(it)
						}
						it.elements.forEach [
							if (elements.contains(componentName + ">" + it.name)) {
								elementsToExpand.add(it)
							}
						]
					]
				}
			]
			return elementsToExpand
		
	}
	/**
	 * Creates
	 */
	private def Set<String> storeExpandedElements() {
		 val Set<String> expandedElements = new TreeSet<String>
		  
		viewer.expandedElements.forEach [
			{
				if (it instanceof String) {
					expandedElements.add(it)
				} else if (it instanceof Component) {
					expandedElements.add((it.eContainer() as AmlModel).package + ">" + it.name)
				} else if (it instanceof ComponentElement) {
					expandedElements.add(
						(it.eContainer().eContainer() as AmlModel).package + ">" +
							(it.eContainer() as Component).name + ">" + it.name)
				} else {
					throw new IllegalArgumentException(
						"unexpected type " + it.class.name + " in expanded TreeElements");
				}
			}
		]
		return expandedElements
	}

	def getNextValue(Iterator<Object> oldExpandedElementsIterator) {
		if (oldExpandedElementsIterator.hasNext) {
			return oldExpandedElementsIterator.next
		}
		return null
	}

	@PreDestroy
	def void preDestroy() {
	}

}
