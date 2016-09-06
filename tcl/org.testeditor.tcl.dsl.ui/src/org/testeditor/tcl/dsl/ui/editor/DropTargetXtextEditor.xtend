package org.testeditor.tcl.dsl.ui.editor

import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.jface.text.source.ISourceViewer
import org.eclipse.swt.custom.StyledText
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.graphics.Point
import org.eclipse.swt.dnd.DropTargetEvent
import org.eclipse.swt.dnd.DropTargetAdapter
import org.eclipse.jface.viewers.ISelection
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.swt.dnd.DropTargetListener
import org.eclipse.ui.dnd.IDragAndDropService
import org.slf4j.LoggerFactory
import org.eclipse.jface.util.LocalSelectionTransfer
import org.eclipse.jface.viewers.TreeSelection
import org.testeditor.aml.InteractionType
import org.testeditor.tcl.TclModel
import org.eclipse.xtext.util.concurrent.IUnitOfWork
import org.eclipse.xtext.resource.XtextResource
import javax.inject.Inject
import org.testeditor.tcl.impl.TclFactoryImpl
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tsl.impl.TslFactoryImpl
import org.testeditor.tsl.StepContentText
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.impl.TemplateTextImpl
import org.testeditor.aml.impl.TemplateVariableImpl
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tcl.StepContentElement
import org.eclipse.jface.text.TextSelection
import org.eclipse.xtext.resource.EObjectAtOffsetHelper
import org.testeditor.tsl.impl.StepContentTextImpl
import org.testeditor.tsl.impl.StepContentVariableImpl
import org.testeditor.tsl.impl.StepContentImpl
import org.testeditor.tcl.impl.StepContentElementImpl
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.aml.impl.AmlFactoryImpl
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.tcl.TestCase

class DropTargetXtextEditor extends XtextEditor {

	static val logger = LoggerFactory.getLogger(DropTargetXtextEditor);
	var ISourceViewer viewer
	@Inject protected TclFactoryImpl tclFactory
	@Inject protected TslFactoryImpl tslFactory
	@Inject protected AmlFactoryImpl amlFactory

	override protected installTextDragAndDrop(ISourceViewer viewer) {
		super.installTextDragAndDrop(viewer)
		this.viewer = viewer

		val fIsTextDragAndDropInstalled = false

		if (viewer == null || fIsTextDragAndDropInstalled)
			return

		val dndService = getSite().getService(IDragAndDropService)
		if (dndService == null)
			return

		val StyledText st = viewer.getTextWidget()
		val DropTargetXtextEditor editor = this
		// Install drag target
		val DropTargetListener dropTargetListener = new DropTargetAdapter() {

			override void dragEnter(DropTargetEvent event) {
				logger.info("dragEnter" + editor.languageName)
				if ("org.testeditor.tcl.dsl.Tcl" != editor.languageName) {
					event.detail = DND.DROP_NONE
				}
				if (interactionType == null) {
					event.detail = DND.DROP_NONE
				}
			}

			override void drop(DropTargetEvent event) {

				val offset = (selectionProvider.selection as TextSelection).offset;
				editor.document.modify(
					new IUnitOfWork<Object, XtextResource>() {

						override exec(XtextResource resource) throws Exception {
							if (resource.contents.get(0) instanceof TclModel) {

								var currentElement = (new EObjectAtOffsetHelper()).resolveElementAt(resource, offset)
								var ComponentTestStepContext testStepContext = null

								val TestCase test = (resource.contents.get(0) as TclModel).test
								var indexOfSelectedTestStep = -1

								if (currentElement == null) {
									if (test.steps.size == 0 ||
										(test.steps.last.contexts.get(0) as ComponentTestStepContext).component.name !=
											component.name) {
										testStepContext = createNewTestContext(-1, test, component)
									} else {
										testStepContext = test.steps.last.contexts.last as ComponentTestStepContext
									}

								} else {
									switch (currentElement.class) {
										case StepContentElementImpl,
										case StepContentVariableImpl,
										case StepContentTextImpl: {
											var selectedTestStep = (currentElement as StepContentImpl).
												eContainer as TestStep
											testStepContext = selectedTestStep.eContainer as ComponentTestStepContext
											indexOfSelectedTestStep = testStepContext.steps.indexOf(
												selectedTestStep) + 1

											if (testStepContext.component.name != component.name) {
												val indexOfContext = test.steps.indexOf(testStepContext.eContainer)
												if (indexOfSelectedTestStep < testStepContext.steps.size()) {
													val splitTestStepContext = createNewTestContext(indexOfContext + 1,
														test, testStepContext.component)
													var stepsBeingMoved = testStepContext.steps.subList(
														indexOfSelectedTestStep, testStepContext.steps.size())
													splitTestStepContext.steps.addAll(stepsBeingMoved)
												}
												testStepContext = createNewTestContext(indexOfContext + 1, test,
													component)
											}
										}
										default:
											throw new IllegalArgumentException(
												"The class '" + currentElement.class.getName() +
													"' is not a valid classifier")
											}
										}
										val TestStep newTestStep = tclFactory.createTestStep
										interactionType.template.contents.forEach [
											switch (it) {
												TemplateText: {
													val StepContentText stepContentText = tslFactory.
														createStepContentText
													stepContentText.value = value
													newTestStep.contents.add(stepContentText)
												}
												TemplateVariable: {
													if (name != 'element') {
														val StepContentVariable stepContentVariable = tslFactory.
															createStepContentVariable
														stepContentVariable.value = name
														newTestStep.contents.add(stepContentVariable)
													} else {
														val StepContentElement stepContentElement = tclFactory.
															createStepContentElement
														stepContentElement.value = componentElement.name
														newTestStep.contents.add(stepContentElement)
													}
												}
												default:
													throw new IllegalArgumentException(
														"The class '" + it.class.getName() +
															"' is not a valid classifier")
													}
												]
												if (indexOfSelectedTestStep < 0 ||
													indexOfSelectedTestStep >= testStepContext.steps.size()) {
													testStepContext.steps.add(newTestStep)
												} else {
													testStepContext.steps.add(indexOfSelectedTestStep, newTestStep)
												}
											}
											return resource
										}

										def ComponentTestStepContext createNewTestContext(int index, TestCase test,
											Component component) {

											var SpecificationStepImplementation specification = tclFactory.
												createSpecificationStepImplementation
											if (index < 0 || index >= test.steps.size()) {
												test.steps.add(specification)
											} else {
												test.steps.add(index, specification)
											}
											specification.contents.add(
												tslFactory.createStepContentText => [
													value = "Kommentar bitte eintragen"
												]
											);
											var componentTestStepContext = tclFactory.createComponentTestStepContext
											componentTestStepContext.component = component
											if (componentElement != null) {
												componentTestStepContext.component.elements.add(componentElement)
											}
											specification.contexts.add(componentTestStepContext)
											return componentTestStepContext
										}

									}
								)
							}

							def ComponentElement getComponentElement() {
								val ISelection sel = LocalSelectionTransfer.getTransfer().getSelection();
								if (sel instanceof TreeSelection) {
									val treeSelection = sel.paths.get(0)
									for (var index = 0; index < treeSelection.segmentCount; index++) {
										if (treeSelection.getSegment(index) instanceof ComponentElement) {
											return treeSelection.getSegment(index) as ComponentElement
										}
									}
								}
								return null
							}

							def Component getComponent() {
								val ISelection sel = LocalSelectionTransfer.getTransfer().getSelection();
								if (sel instanceof TreeSelection) {
									val treeSelection = sel.paths.get(0)
									for (var index = 0; index < treeSelection.segmentCount; index++) {
										if (treeSelection.getSegment(index) instanceof Component) {
											return treeSelection.getSegment(index) as Component
										}
									}
								}
								return null
							}

							def InteractionType getInteractionType() {
								val ISelection sel = LocalSelectionTransfer.getTransfer().getSelection();
								if (sel instanceof TreeSelection) {
									val treeSelection = sel.paths.get(0)
									for (var index = 0; index < treeSelection.segmentCount; index++) {
										if (treeSelection.getSegment(index) instanceof InteractionType) {
											return treeSelection.getSegment(index) as InteractionType
										}
									}
								}
								return null
							}
						};
						dndService.addMergedDropTarget(st, DND.DROP_MOVE.bitwiseOr(DND.DROP_COPY),
							#[TextTransfer.instance], dropTargetListener);
					}

				}
				