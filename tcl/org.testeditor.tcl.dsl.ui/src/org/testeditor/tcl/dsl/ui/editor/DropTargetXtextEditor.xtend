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
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.AbstractTestStep
import org.eclipse.xtext.xtype.XImportSection
import org.eclipse.xtext.xtype.XImportDeclaration
import org.eclipse.jface.text.source.SourceViewer
import org.eclipse.emf.ecore.EObject
import org.testeditor.tsl.StepContent
import org.testeditor.tcl.impl.TestStepImpl
import org.eclipse.xtext.EcoreUtil2
import java.util.List
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.resource.ILocationInFileProvider

class DropTargetXtextEditor extends XtextEditor {

	var ISourceViewer viewer
	@Inject protected TclFactoryImpl tclFactory
	@Inject protected TslFactoryImpl tslFactory

	@Inject extension EObjectAtOffsetHelper
	@Inject extension ILocationInFileProvider
	@Inject DropTargetXtextEditorListener dropTargetListener

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
		dropTargetListener.editor = this
		val DropTargetListener dropTargetListener = new DropTargetAdapter() {

			override void dragEnter(DropTargetEvent event) {
				if ("org.testeditor.tcl.dsl.Tcl" != editor.languageName) {
					event.detail = DND.DROP_NONE
				}
				if (getSelectionAs(InteractionType) == null) {
					event.detail = DND.DROP_NONE
				}
			}

			override void drop(DropTargetEvent event) {

				val List<String> toFormat = newArrayList
				val List<String> currentElement = newArrayList

				editor.document.modify(
					updateModel(toFormat, currentElement)
				)
				editor.document.modify(
					formatRelevantRegion(toFormat)
				)
				editor.document.modify(
					setCursorToNewElement(currentElement)
				)

			}

			def updateModel(List<String> toFormat, List<String> currentElement) {
				new IUnitOfWork.Void<XtextResource>() {

					override process(XtextResource resource) throws Exception {
						if (resource.contents.head instanceof TclModel) {
							val tclModel = resource.contents.head as TclModel
							val List<EObject> toFormatEObject = newArrayList
							// testStepContext, idx = determineInsertionPoint()
							// insertionPoint.index = indexOfSelectedTestStep
							// insertionPoint.testStepContext = testStepContext
							val insertionData = determineInsertionData(resource, toFormatEObject)
							toFormatEObject.add(insertionData.testStepContext)

							val TestStep newTestStep = createNewTestStep
							addTestStepToModel(insertionData, newTestStep)

							currentElement.add(EcoreUtil.getRelativeURIFragmentPath(tclModel, newTestStep))
							toFormat.addAll(toFormatEObject.map[EcoreUtil.getRelativeURIFragmentPath(tclModel, it)])
						}
					}

					def addTestStepToModel(ModelInsertData insertionPoint, TestStep newTestStep) {
						if (insertionPoint.index < 0 ||
							insertionPoint.index >= insertionPoint.testStepContext.steps.size()) {
							insertionPoint.testStepContext.steps.add(newTestStep)
						} else {
							insertionPoint.testStepContext.steps.add(insertionPoint.index, newTestStep)
						}
					}

					def createNewTestStep() {
						val newTestStep = tclFactory.createTestStep

						getSelectionAs(InteractionType).template.contents.forEach [
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
										val StepContentElement stepContentElement = tclFactory.createStepContentElement
										stepContentElement.value = getSelectionAs(ComponentElement).name
										newTestStep.contents.add(stepContentElement)
									}
								}
								default:
									throw new IllegalArgumentException("The class '" + it.class.getName() +
										"' is not a valid classifier")
							}
						]
						return newTestStep
					}

				}
			}

			def ModelInsertData determineInsertionData(XtextResource resource, List<EObject> toFormatEObject) {
				val offset = (selectionProvider.selection as TextSelection).offset;
				var contained = resolveContainedElementAt(resource, offset)
				val tclModel = resource.contents.head as TclModel
				var ComponentTestStepContext testStepContext = null

				val TestCase test = (resource.contents.head as TclModel).test
				var indexOfSelectedTestStep = 0

				testStepContext = EcoreUtil2.getContainerOfType(contained, ComponentTestStepContext)

				if (testStepContext == null) {
					val insertionIndex = if (contained == null) {
							val prev = tclModel.test.steps.last?.contexts?.last
							if (prev !== null) {
								toFormatEObject.add(prev)
							} else {
								toFormatEObject.add(tclModel)
							};
							-1
						} else {
							0
						}
					testStepContext = createNewTestContext(insertionIndex, test, getSelectionAs(Component))
				} else {

					var AbstractTestStep selectedTestStep = null
					switch (contained) {
						AbstractTestStep:
							selectedTestStep = contained
						XImportSection,
						XImportDeclaration,
						TestCase,
						TclModel:
							testStepContext = createNewTestContext(0, test, getSelectionAs(Component))
						default:
							selectedTestStep = EcoreUtil2.getContainerOfType(contained, AbstractTestStep)
					}
					if (selectedTestStep != null) {
						testStepContext = selectedTestStep.eContainer as ComponentTestStepContext
						indexOfSelectedTestStep = testStepContext.steps.indexOf(selectedTestStep) + 1
					}

					if (testStepContext.component.name != getSelectionAs(Component).name) {
						val indexOfContext = test.steps.indexOf(testStepContext.eContainer)
						if (indexOfSelectedTestStep > 0 && indexOfSelectedTestStep < testStepContext.steps.size()) {
							val splitTestStepContext = createNewTestContext(indexOfContext + 1, test,
								testStepContext.component)
							var stepsBeingMoved = testStepContext.steps.subList(indexOfSelectedTestStep,
								testStepContext.steps.size())
							splitTestStepContext.steps.addAll(stepsBeingMoved)
							toFormatEObject.add(splitTestStepContext.steps.head);
							toFormatEObject.add(testStepContext.steps.last);
						}
						if (indexOfSelectedTestStep == 0) {
							testStepContext = createNewTestContext(indexOfContext, test, getSelectionAs(Component))
						} else {
							testStepContext = createNewTestContext(indexOfContext + 1, test, getSelectionAs(Component))
						}
					}
				}

				return new ModelInsertData(testStepContext, indexOfSelectedTestStep)
			}

			def ComponentTestStepContext createNewTestContext(int index, TestCase test, Component component) {

				var SpecificationStepImplementation specification = tclFactory.createSpecificationStepImplementation
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
				if (getSelectionAs(ComponentElement) != null) {
					componentTestStepContext.component.elements.add(getSelectionAs(ComponentElement))
				}
				specification.contexts.add(componentTestStepContext)
				return componentTestStepContext
			}

			def setCursorToNewElement(List<String> currentElement) {
				new IUnitOfWork.Void<XtextResource>() {

					override process(XtextResource resource) throws Exception {

						if (resource.contents.get(0) instanceof TclModel) {
							val eObject = EcoreUtil2.getEObject(resource.contents.head, currentElement.head)
							var currentRegion = eObject.fullTextRegion;
							(internalSourceViewer as SourceViewer).setSelectedRange(currentRegion.offset +
								currentRegion.length, 0)
							editor.setFocus

						}
					}

				}
			}

			def formatRelevantRegion(List<String> toFormat) {
				new IUnitOfWork.Void<XtextResource>() {

					override process(XtextResource resource) throws Exception {

						if (resource.contents.get(0) instanceof TclModel) {

							val textRegion = toFormat //
							.map[EcoreUtil2.getEObject(resource.contents.head, it)] //
							.map[fullTextRegion] //
							.reduce[textRegion1, textRegion2|textRegion1.merge(textRegion2)]

							(internalSourceViewer as SourceViewer) => [
								setSelectedRange(textRegion.offset, textRegion.length)
								doOperation(ISourceViewer.FORMAT)
							]
						}
					}

				}
			}

			def ComponentTestStepContext getTestStepContext(EObject currentElement, EObject containedElement,
				TestCase test) {
				var ComponentTestStepContext testStepContext = null

				if (currentElement == null) {
					if (test.steps.size == 0 ||
						(test.steps.last.contexts.get(0) as ComponentTestStepContext).component.name !=
							getSelectionAs(Component).name) {
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
								testStepContext = (currentElement.eContainer as SpecificationStepImplementation).
									contexts.head as ComponentTestStepContext
							} else {
								testStepContext = currentElement.eContainer.eContainer as ComponentTestStepContext
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

			def <T> T getSelectionAs(Class<T> clazz) {
				val ISelection sel = LocalSelectionTransfer.getTransfer().getSelection();
				if (sel instanceof TreeSelection) {
					val treeSelection = sel.paths.head
					for (var index = 0; index < treeSelection.segmentCount; index++) {
						if (clazz.isInstance(treeSelection.getSegment(index))) {
							return treeSelection.getSegment(index) as T
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
