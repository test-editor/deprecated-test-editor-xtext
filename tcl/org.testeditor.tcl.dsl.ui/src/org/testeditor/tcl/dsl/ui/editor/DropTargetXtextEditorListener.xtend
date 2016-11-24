package org.testeditor.tcl.dsl.ui.editor

import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.swt.dnd.DropTargetAdapter
import org.eclipse.swt.dnd.DropTargetEvent
import org.eclipse.xtext.resource.XtextResource
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentVariable
import org.testeditor.aml.Component
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.jface.text.source.SourceViewer
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.util.LocalSelectionTransfer
import org.eclipse.jface.viewers.TreeSelection
import org.eclipse.jface.text.source.ISourceViewer
import javax.inject.Inject
import org.eclipse.swt.dnd.DND
import org.eclipse.jface.text.TextSelection
import org.eclipse.xtext.resource.ILocationInFileProvider
import org.eclipse.xtend.lib.annotations.Accessors
import org.testeditor.tsl.impl.StepContentTextImpl
import org.testeditor.tcl.impl.SpecificationStepImplementationImpl
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.testeditor.tcl.TclFactory
import org.testeditor.tsl.TslFactory

class DropTargetXtextEditorListener extends DropTargetAdapter {

	static val private TclFactory tclFactory = TclFactory.eINSTANCE
	static val private TslFactory tslFactory = TslFactory.eINSTANCE

	@Inject extension ILocationInFileProvider
	@Inject protected ContentAssistContext.Factory contentAssistFactory

	@Accessors(PUBLIC_SETTER)
	private DropTargetXtextEditor editor

	override void dragEnter(DropTargetEvent event) {
		if ("org.testeditor.tcl.dsl.Tcl" != editor.languageName) {
			event.detail = DND.DROP_NONE
		}
		if (getDroppedObjectAs(InteractionType) == null) {
			event.detail = DND.DROP_NONE
		}
	}

	override void drop(DropTargetEvent event) {

		val List<String> toFormat = newArrayList
		val List<String> currentElement = newArrayList

		editor.document.modify[updateModel(toFormat, currentElement)]
		editor.document.modify [
			formatRelevantRegion(toFormat)
		]
		editor.document.modify [
			setCursorToNewElement(currentElement)
		]

	}

	private def updateModel(XtextResource resource, List<String> toFormat, List<String> currentElement) {

		val tclModel = resource.contents.head
		if (tclModel instanceof TclModel) {
			val toFormatEObject = newArrayList
			val dropTarget = findDropTarget(resource)

			val ComponentTestStepContext droppedTestStepContext = createDroppedTestStepContext(
				getDroppedObjectAs(InteractionType), getDroppedObjectAs(ComponentElement),
				getDroppedObjectAs(Component))
				val droppedTestStep = droppedTestStepContext.steps.head

				updateTestModel(droppedTestStepContext, tclModel.test, dropTarget, toFormatEObject)

				toFormat.addAll(toFormatEObject.map[EcoreUtil.getRelativeURIFragmentPath(tclModel, it)])
				currentElement.add(EcoreUtil.getRelativeURIFragmentPath(tclModel, droppedTestStep))
			}
		}

		public def void updateTestModel(ComponentTestStepContext droppedTestStepContext, TestCase test,
			EObject dropTarget, List<EObject> toFormatEObject) {

			var insertionIndex = 0
			var ComponentTestStepContext targetTestStepContext = searchTargetTestStepContext(test, dropTarget)

			if (targetTestStepContext == null) {
				insertTargetTestStepContext(test, droppedTestStepContext, dropTarget, 0, toFormatEObject)
				toFormatEObject.add(droppedTestStepContext)
			} else {
				insertionIndex = getInsertionIndex(targetTestStepContext, dropTarget)

				if (targetTestStepContext.component.name != droppedTestStepContext.component.name) {
					var targetTestStepContextIndex = (targetTestStepContext.
						eContainer as SpecificationStepImplementation).contexts.indexOf(targetTestStepContext)

					// Insert in the middle of an existing TestStepContext
					if (insertionIndex > 0 && insertionIndex < targetTestStepContext.steps.size()) {
						splitedTargetTestStepContext(targetTestStepContext, targetTestStepContextIndex, insertionIndex,
							toFormatEObject)
					}
					// If it is not dropped at the top, insert new TestSepContext after the existing TestStepContext
					if (insertionIndex > 0) {
						targetTestStepContextIndex++
						toFormatEObject.add(targetTestStepContext.steps.last)
					}
					insertTargetTestStepContext(test, droppedTestStepContext, dropTarget, targetTestStepContextIndex,
						toFormatEObject)
					toFormatEObject.add(droppedTestStepContext)
				} else {
					addTestStepToModel(insertionIndex, targetTestStepContext, droppedTestStepContext.steps.head)
					toFormatEObject.add(targetTestStepContext)
				}
			}

		}

		private def findDropTarget(XtextResource resource) {
			val offset = (editor.selectionProvider.selection as TextSelection).offset
			return contentAssistFactory.create(editor.internalSourceViewer, offset, resource).head.currentModel
		}

		private def addTestStepToModel(int insertionIndex, ComponentTestStepContext testStepContext,
			AbstractTestStep droppedTestStep) {
			if (insertionIndex < 0 || insertionIndex >= testStepContext.steps.size()) {
				testStepContext.steps.add(droppedTestStep)
			} else {
				testStepContext.steps.add(insertionIndex, droppedTestStep)
			}
		}

		public def ComponentTestStepContext createDroppedTestStepContext(InteractionType interactionType,
			ComponentElement componentElement, Component component) {
			val componentTestStepContext = tclFactory.createComponentTestStepContext
			componentTestStepContext.component = component
			componentTestStepContext.steps.add(createDroppedTestStep(interactionType, componentElement))
			return componentTestStepContext
		}

		private def createDroppedTestStep(InteractionType interactionType, ComponentElement componentElement) {
			val newTestStep = tclFactory.createTestStep

			interactionType.template.contents.forEach [
				switch (it) {
					TemplateText: {
						val StepContentText stepContentText = tslFactory.createStepContentText
						stepContentText.value = value
						newTestStep.contents.add(stepContentText)
					}
					TemplateVariable: {
						if (name != 'element') {
							val StepContentVariable stepContentVariable = tslFactory.createStepContentVariable
							stepContentVariable.value = name
							newTestStep.contents.add(stepContentVariable)
						} else {
							val StepContentElement stepContentElement = tclFactory.createStepContentElement
							stepContentElement.value = componentElement.name
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

		private def ComponentTestStepContext searchTargetTestStepContext(TestCase test, EObject dropTarget) {

			if (dropTarget == null) {
				if (test.steps.empty) { // empty
					return null
				}
				return test.steps.last.contexts.last as ComponentTestStepContext
			}
			if (dropTarget instanceof SpecificationStepImplementationImpl) {
				return dropTarget.getContexts.head as ComponentTestStepContext
			}
			if (dropTarget instanceof StepContentTextImpl &&
				dropTarget.eContainer instanceof SpecificationStepImplementationImpl) {
				return (dropTarget.eContainer as SpecificationStepImplementationImpl).getContexts().
					head as ComponentTestStepContext
			}

			return EcoreUtil2.getContainerOfType(dropTarget, ComponentTestStepContext)
		}

		private def int getInsertionIndex(ComponentTestStepContext testStepContext, EObject dropTarget) {
			if (dropTarget == null) {
				return testStepContext.steps.size
			}
			var AbstractTestStep selectedTestStep = EcoreUtil2.getContainerOfType(dropTarget, AbstractTestStep)
			return testStepContext.steps.indexOf(selectedTestStep) + 1
		}

		private def setCursorToNewElement(XtextResource resource, List<String> currentElement) {
			val tclModel = resource.contents.head
			if (tclModel instanceof TclModel) {
				val eObject = EcoreUtil2.getEObject(tclModel, currentElement.head)
				var currentRegion = eObject.fullTextRegion;

				(editor.internalSourceViewer as SourceViewer).setSelectedRange(currentRegion.offset +
					currentRegion.length, 0)
				editor.setFocus

			}
			return editor
		}

		private def void insertTargetTestStepContext(TestCase test, ComponentTestStepContext droppedTestStepContext,
			EObject dropTarget, int contextIndex, List<EObject> toFormatEObject) {

			var SpecificationStepImplementation specification = null
			if (test.steps.size() == 0) {
				specification = tclFactory.createSpecificationStepImplementation
				toFormatEObject.add(specification)
				test.steps.add(specification)
			} else if (test.steps.last.contexts.size() == 0) {
				specification = test.steps.last
				toFormatEObject.add(specification)
			} else {
				if (dropTarget == null) {
					specification = test.steps.last
					toFormatEObject.add(specification.contexts.last)
				} else {
					specification = EcoreUtil2.getContainerOfType(dropTarget, SpecificationStepImplementation) ?:
						test.steps.head
				}
			}
			specification.contexts.add(contextIndex, droppedTestStepContext)
		}

		private def splitedTargetTestStepContext(ComponentTestStepContext targetTestStepContext,
			int targetTestStepContextIndex, int insertionIndex, List<EObject> toFormatEObject) {

			var newComponentTestStepContext = tclFactory.createComponentTestStepContext
			newComponentTestStepContext.component = targetTestStepContext.component
			var specification = EcoreUtil2.getContainerOfType(targetTestStepContext, SpecificationStepImplementation)

			specification.contexts.add(targetTestStepContextIndex + 1, newComponentTestStepContext)

			var stepsBeingMoved = targetTestStepContext.steps.subList(insertionIndex,
				targetTestStepContext.steps.size())
			newComponentTestStepContext.steps.addAll(stepsBeingMoved)

			toFormatEObject.add(targetTestStepContext.steps.last)
			toFormatEObject.add(newComponentTestStepContext.steps.head)
		}

		private def formatRelevantRegion(XtextResource resource, List<String> toFormat) {

			val tclModel = resource.contents.head
			if (tclModel instanceof TclModel) {

				val textRegion = toFormat //
				.map[EcoreUtil2.getEObject(tclModel, it)] //
				.map[fullTextRegion] //
				.reduce[textRegion1, textRegion2|textRegion1.merge(textRegion2)]

				(editor.internalSourceViewer as SourceViewer) => [
					setSelectedRange(textRegion.offset, textRegion.length)
					doOperation(ISourceViewer.FORMAT)
				]
			}
		}

		private def <T> T getDroppedObjectAs(Class<T> clazz) {
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

	}
	