package org.testeditor.tcl.dsl.ui.editor

import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.util.LocalSelectionTransfer
import org.eclipse.jface.viewers.TreeSelection
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import javax.inject.Inject
import org.eclipse.jface.text.TextSelection
import org.testeditor.tcl.TclFactory
import org.testeditor.tsl.TslFactory
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.aml.Component
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.TemplateText
import org.testeditor.tsl.StepContentText
import org.testeditor.aml.TemplateVariable
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.impl.SpecificationStepImplementationImpl
import org.testeditor.tsl.impl.StepContentTextImpl
import org.eclipse.xtext.EcoreUtil2
import org.testeditor.tcl.TestCase
import org.eclipse.emf.ecore.EObject
import org.testeditor.aml.AmlFactory
import org.eclipse.xtext.xtype.XtypeFactory
import org.eclipse.xtext.xtype.XImportSection
import org.eclipse.xtext.xtype.XImportDeclaration

class DropUtils {

	static val private TclFactory tclFactory = TclFactory.eINSTANCE
	static val private TslFactory tslFactory = TslFactory.eINSTANCE
    static val private XtypeFactory xtypeFactory = XtypeFactory.eINSTANCE

	@Inject protected ContentAssistContext.Factory contentAssistFactory

	protected def <T> T getDroppedObjectAs(Class<T> clazz) {
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

	protected def findDropTarget(DropTargetXtextEditor editor, XtextResource resource) {
		val offset = (editor.selectionProvider.selection as TextSelection).offset
		return contentAssistFactory.create(editor.internalSourceViewer, offset, resource).head.currentModel
	}

	public def ComponentTestStepContext createDroppedTestStepContext() {
		return createDroppedTestStepContext(getDroppedObjectAs(Component), getDroppedObjectAs(ComponentElement),
			getDroppedObjectAs(InteractionType))
	}

	public def ComponentTestStepContext createDroppedTestStepContext(Component component,
		ComponentElement componentElement, InteractionType interactionType) {

		val componentTestStepContext = tclFactory.createComponentTestStepContext
		componentTestStepContext.component = component
		componentTestStepContext.steps.add(createDroppedTestStep(interactionType, componentElement))
		return componentTestStepContext
	}

	public def createDroppedTestStep(InteractionType interactionType, ComponentElement componentElement) {
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

	protected def SpecificationStepImplementation createSpecification() {
		return tclFactory.createSpecificationStepImplementation
	}

	protected def createComponentTestStepContext() {
		return tclFactory.createComponentTestStepContext
	}

	protected def XImportDeclaration createXImportDeclaration() {
		return xtypeFactory.createXImportDeclaration
	}
	protected def XImportSection createXImportSection() {
		return xtypeFactory.createXImportSection
	}

	protected def addTestStepToModel(int insertionIndex, ComponentTestStepContext testStepContext,
		AbstractTestStep droppedTestStep) {
		if (insertionIndex < 0 || insertionIndex >= testStepContext.steps.size()) {
			testStepContext.steps.add(droppedTestStep)
		} else {
			testStepContext.steps.add(insertionIndex, droppedTestStep)
		}
	}

	protected def ComponentTestStepContext searchTargetTestStepContext(TestCase test, EObject dropTarget) {

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

	protected def int getInsertionIndex(ComponentTestStepContext testStepContext, EObject dropTarget) {
		if (dropTarget == null) {
			return testStepContext.steps.size
		}
		var AbstractTestStep selectedTestStep = EcoreUtil2.getContainerOfType(dropTarget, AbstractTestStep)
		return testStepContext.steps.indexOf(selectedTestStep) + 1
	}

}
