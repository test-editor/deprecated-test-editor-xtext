package org.testeditor.tcl.dsl.ui.editor

import org.eclipse.xtext.ui.editor.XtextEditor
import org.eclipse.jface.text.source.ISourceViewer
import org.eclipse.swt.custom.StyledText
import org.eclipse.swt.dnd.DND
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
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tcl.StepContentElement
import org.eclipse.jface.text.TextSelection
import org.eclipse.xtext.resource.EObjectAtOffsetHelper
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.aml.impl.AmlFactoryImpl
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.StringConstant
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.tcl.Comparator
import org.eclipse.xtext.xtype.XImportDeclaration
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.TestStepWithAssignment
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.jface.text.source.SourceViewer
import org.eclipse.emf.ecore.EObject
import org.testeditor.tsl.StepContent
import org.testeditor.tcl.impl.TestStepImpl
import org.eclipse.xtext.EcoreUtil2
import org.testeditor.tcl.TestStepContext
import java.util.List
import org.eclipse.xtext.nodemodel.ICompositeNode
import java.util.concurrent.atomic.AtomicLong

class DropTargetXtextEditor extends XtextEditor {

	static val logger = LoggerFactory.getLogger(DropTargetXtextEditor);
	var ISourceViewer viewer
	@Inject protected TclFactoryImpl tclFactory
	@Inject protected TslFactoryImpl tslFactory
	@Inject protected AmlFactoryImpl amlFactory

	@Inject extension EObjectAtOffsetHelper

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
				val List<EObject> toFormat = newArrayList

				editor.document.modify(
					new IUnitOfWork.Void<XtextResource>() {

						override process(XtextResource resource) throws Exception {
							if (resource.contents.head instanceof TclModel) {

								var contained = resolveContainedElementAt(resource, offset)

								var ComponentTestStepContext testStepContext = null

								val TestCase test = (resource.contents.head as TclModel).test
								var indexOfSelectedTestStep = 0

								testStepContext = EcoreUtil2.getContainerOfType(contained, ComponentTestStepContext)

								if (testStepContext == null) {
									val insertionIndex = if (contained == null) {
											-1
										} else {
											0
										}
									testStepContext = createNewTestContext(insertionIndex, test, component)
								} else {

									var AbstractTestStep selectedTestStep = null
									switch (contained) {
										AbstractTestStep:
											selectedTestStep = contained
										XImportSection,
										XImportDeclaration,
										TestCase,
										TclModel:
											testStepContext = createNewTestContext(0, test, component)
										default:
											selectedTestStep = EcoreUtil2.getContainerOfType(contained,
												AbstractTestStep)
									}
									if (selectedTestStep != null) {
										testStepContext = selectedTestStep.eContainer as ComponentTestStepContext
										indexOfSelectedTestStep = testStepContext.steps.indexOf(selectedTestStep) + 1
									}

									if (testStepContext.component.name != component.name) {
										val indexOfContext = test.steps.indexOf(testStepContext.eContainer)
										if (indexOfSelectedTestStep > 0 &&
											indexOfSelectedTestStep < testStepContext.steps.size()) {
											val splitTestStepContext = createNewTestContext(indexOfContext + 1, test,
												testStepContext.component)
											var stepsBeingMoved = testStepContext.steps.subList(indexOfSelectedTestStep,
												testStepContext.steps.size())
											splitTestStepContext.steps.addAll(stepsBeingMoved)
											toFormat.add(splitTestStepContext)
											val node = NodeModelUtils.findActualNodeFor(splitTestStepContext)
										val reformatOffset = node.getTotalOffset() //
										val reformatMaxOffset = node.getTotalLength() + reformatOffset - 1
										}
										if (indexOfSelectedTestStep == 0) {
											testStepContext = createNewTestContext(indexOfContext, test, component)
										} else {
											testStepContext = createNewTestContext(indexOfContext + 1, test, component)
										}
									}
									toFormat.add(testStepContext)
								}
								toFormat.add(testStepContext)
								val TestStep newTestStep = tclFactory.createTestStep
								interactionType.template.contents.forEach [
									switch (it) {
										TemplateText: {
											val StepContentText stepContentText = tslFactory.createStepContentText
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
											throw new IllegalArgumentException("The class '" + it.class.getName() +
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
				editor.document.modify(
					new IUnitOfWork.Void<XtextResource>() {

						override process(XtextResource resource) throws Exception {

							if (resource.contents.get(0) instanceof TclModel) {
								var int minOffset = Integer.MAX_VALUE
								var int maxOffset = 0
								for (eObject : toFormat) {
									val node = NodeModelUtils.findActualNodeFor(eObject)
									if (node != null) {
										val reformatOffset = node.getTotalOffset() //
										val reformatMaxOffset = node.getTotalLength() + reformatOffset - 1
										minOffset = Math.min(minOffset, reformatOffset)
										maxOffset = Math.max(maxOffset, reformatMaxOffset)
									}
								}
								val int reformatOffset = minOffset
								val int reformatLength = maxOffset - minOffset + 1
								(internalSourceViewer as SourceViewer) => [
									setSelectedRange(0, editor.document.length - 1)
									doOperation(ISourceViewer.FORMAT); // execute format on selection
									editor.setFocus
								]

//								toFormat.forEach [
//									val node = NodeModelUtils.findActualNodeFor(it) // find node for model element that should be reformatted
//									if (node != null) {
//										val reformatLength = node.getTotalLength() // get node text position in editor
//										val reformatOffset = node.getTotalOffset() //
//										editor.document.modify(
//											new IUnitOfWork<Object, XtextResource> {
//												override exec(XtextResource state) throws Exception {
//													(internalSourceViewer as SourceViewer) =>
//														[
//															val savedSelection = selection // keep old selection
//															val tobeformatted = internalSourceViewer.document.get(
//																reformatOffset, reformatLength)
//															setSelectedRange(reformatOffset, reformatLength)
//															doOperation(ISourceViewer.FORMAT); // execute format on selection
//															setSelection(savedSelection, false)
//															editor.setFocus
//														]
//												}
//											}
//										)
//									}
//								]
							}
						}

					}
				)

			}

			def ComponentTestStepContext getTestStepContext(EObject currentElement, EObject containedElement,
				TestCase test) {
				var ComponentTestStepContext testStepContext = null

				if (currentElement == null) {
					if (test.steps.size == 0 ||
						(test.steps.last.contexts.get(0) as ComponentTestStepContext).component.name !=
							component.name) {
								return null
							} else {
								testStepContext = test.steps.last.contexts.last as ComponentTestStepContext
							}

						} else {
							switch (currentElement) {
								TestStepImpl:
									testStepContext = currentElement.eContainer as ComponentTestStepContext
								StepContent: {
									if (currentElement.eContainer instanceof SpecificationStepImplementation) {
										testStepContext = (currentElement.
											eContainer as SpecificationStepImplementation).contexts.
											head as ComponentTestStepContext
									} else {
										testStepContext = currentElement.eContainer.
											eContainer as ComponentTestStepContext
									}
								}
								ComponentTestStepContext:
									testStepContext = currentElement
								SpecificationStepImplementation:
									testStepContext = currentElement.contexts.head as ComponentTestStepContext
								Component:
									testStepContext = containedElement as ComponentTestStepContext
							}

						}
						return testStepContext

					}

					def ComponentElement getComponentElement() {
						val ISelection sel = LocalSelectionTransfer.getTransfer().getSelection();
						if (sel instanceof TreeSelection) {
							val treeSelection = sel.paths.head
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
							val treeSelection = sel.paths.head
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
				dndService.addMergedDropTarget(st, DND.DROP_MOVE.bitwiseOr(DND.DROP_COPY), #[TextTransfer.instance],
					dropTargetListener);
			}

		}
		