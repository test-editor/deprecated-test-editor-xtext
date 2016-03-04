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

import javax.annotation.PostConstruct
import javax.annotation.PreDestroy
import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Optional
import org.eclipse.e4.core.di.extensions.EventTopic
import org.eclipse.e4.ui.di.Focus
import org.eclipse.e4.ui.workbench.UIEvents
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.viewers.TreeViewer
import org.eclipse.swt.SWT
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.DragSourceEvent
import org.eclipse.swt.dnd.DragSourceListener
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Display
import org.slf4j.LoggerFactory

import static org.testeditor.rcp4.views.teststepselector.XtendSWTLib.*

/** 
 * part that display a tree view with drag and drop elements of the aml model which can be inserted into
 * a tcl document
 */
class TestStepSelector {

	public static val String SELECTOR_TOPIC_REFRESH = "MaskStepSelector_Refresh"

	val logger = LoggerFactory.getLogger(TestStepSelector);

	@Inject AmlInjectorProvider amlInjectorProvider
	@Inject TestStepSelectorLabelProvider labelProvider
	@Inject TestStepSelectorDropTextProvider dropTextProvider
	@Inject AmlDropSupport amlDropSupport

	/** set as soon as the view is populated */
	boolean populated = false
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
	}

	@Inject
	@Optional
	def void startupComplete(@EventTopic(UIEvents.UILifeCycle.APP_STARTUP_COMPLETE) Object data) {
		// startup complete ensures the index to be populated
		Display.getDefault.syncExec[populateViewIfEmpty] // is only run, if view is created on startup
	}

	@Focus
	def void setFocus() {
		populateViewIfEmpty
		viewer.control.setFocus
	}

	@Inject
	@Optional
	def void refreshView(@EventTopic(SELECTOR_TOPIC_REFRESH) Object data) {
		populated = false
		logger.debug("Refreshing view.")
		Display.getDefault.syncExec[populateViewIfEmpty]
	}

	// make sure that the index is populated before calling this method
	// and that it is run in the swt thread!
	private def populateViewIfEmpty() {
		if (populated) {
			return
		}
		val amlInjector = amlInjectorProvider.get

		populated = true
		viewer.labelProvider = labelProvider
		viewer.contentProvider = amlInjector.getInstance(TestStepSelectorTreeContentProvider)

		val amlModelsProvider = amlInjector.getInstance(AmlModelsProvider)
		viewer.input = amlModelsProvider.amlModels
		viewer.expandAll
	}

	@PreDestroy
	def void preDestroy() {
	}

}
