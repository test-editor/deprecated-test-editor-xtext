package org.testeditor.tcl.dsl.ui.editor

import com.google.inject.Inject
import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.resource.XtextResource
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.StepContainer
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.MacroTestStepContext

class UpdateTestModelByDropTarget {

	@Inject private DropUtils dropUtils

	protected def updateModel(XtextResource resource, DropTargetXtextEditor editor, List<String> toFormat,
		List<String> currentElement) {

		val tclModel = resource.contents.head
		val toFormatEObject = newArrayList

		if (tclModel instanceof TclModel) {
			val dropTarget = dropUtils.findDropTarget(editor, resource)

			val ComponentTestStepContext newTestStepContext = dropUtils.createDroppedTestStepContext()
			val newTestStep = newTestStepContext.steps.head

			updateTestModel(tclModel, dropTarget, newTestStepContext, toFormatEObject)

			toFormat.addAll(toFormatEObject.map[EcoreUtil.getRelativeURIFragmentPath(tclModel, it)])
			currentElement.add(EcoreUtil.getRelativeURIFragmentPath(tclModel, newTestStep))
		}
	}

	public def void updateTestModel(TclModel tclModel, EObject dropTarget, ComponentTestStepContext newTestStepContext,
		List<EObject> toFormatEObject) {

		var testStepIndex = 0

		val ComponentTestStepContext targetTestStepContext = dropUtils.searchTargetTestStepContext(tclModel, dropTarget)

		if (targetTestStepContext === null) {
			insertTargetTestStepContext(tclModel, newTestStepContext, dropTarget, -1, toFormatEObject)
			toFormatEObject.add(newTestStepContext)
		} else {
			testStepIndex = dropUtils.getInsertionIndex(targetTestStepContext, dropTarget)

			if (targetTestStepContext.component.name != newTestStepContext.component.name) {
				var targetTestStepContextIndex = (targetTestStepContext.eContainer as StepContainer).contexts.indexOf(
					targetTestStepContext)

				// Insert in the middle of an existing TestStepContext
				if (testStepIndex > 0 && testStepIndex < targetTestStepContext.steps.size()) {
					splitedTargetTestStepContext(targetTestStepContext, targetTestStepContextIndex, testStepIndex,
						toFormatEObject)
				}
				// If it is not dropped at the top, insert new TestSepContext after the existing TestStepContext
				if (testStepIndex > 0) {
					targetTestStepContextIndex++
					toFormatEObject.add(targetTestStepContext.steps.last)
				}
				insertTargetTestStepContext(tclModel, newTestStepContext, dropTarget, targetTestStepContextIndex,
					toFormatEObject)
				toFormatEObject.add(newTestStepContext)
			} else {
				dropUtils.addTestStepToModel(testStepIndex, targetTestStepContext, newTestStepContext.steps.head)
				toFormatEObject.add(targetTestStepContext)
			}

		}
	}

	private def void insertTargetTestStepContext(TclModel tclModel, ComponentTestStepContext droppedTestStepContext,
		EObject dropTarget, int contextIndex, List<EObject> toFormatEObject) {

		var StepContainer specification = null
		if (tclModel.test != null) {
			if (tclModel.test.steps.size() == 0) {
				tclModel.test.steps.add(dropUtils.createSpecification)
			}
			specification = EcoreUtil2.getContainerOfType(dropTarget, StepContainer) ?: tclModel.test.steps.last
		}
		if (tclModel.macroCollection != null) {

			if (tclModel.macroCollection.macros.size() == 0) {
				tclModel.macroCollection.macros.add(dropUtils.createMacro)
			}
			specification = EcoreUtil2.getContainerOfType(dropTarget, StepContainer) ?:
				tclModel.macroCollection.macros.last
		}

		if (specification.contexts.size() == 0) {
			toFormatEObject.add(specification)
		}
		if (contextIndex < 0) {
			// Call to a macro at the end of the testcase - add at the end
			if (EcoreUtil2.getContainerOfType(dropTarget, MacroTestStepContext) != null) {
				if (specification.contexts.size > 0) {
					toFormatEObject.add(specification.contexts.last.steps.last)
				}
				specification.contexts.add(droppedTestStepContext)
			} else { // drop on the import section. Add at the beginning
				specification.contexts.add(0, droppedTestStepContext)
			}
		} else {
			specification.contexts.add(contextIndex, droppedTestStepContext)
		}
	}

	private def splitedTargetTestStepContext(ComponentTestStepContext targetTestStepContext,
		int targetTestStepContextIndex, int insertionIndex, List<EObject> toFormatEObject) {

		val newComponentTestStepContext = dropUtils.createComponentTestStepContext
		newComponentTestStepContext.component = targetTestStepContext.component
		val specification = EcoreUtil2.getContainerOfType(targetTestStepContext, StepContainer)

		specification.contexts.add(targetTestStepContextIndex + 1, newComponentTestStepContext)

		val stepsBeingMoved = targetTestStepContext.steps.subList(insertionIndex, targetTestStepContext.steps.size())
		newComponentTestStepContext.steps.addAll(stepsBeingMoved)

		toFormatEObject.add(targetTestStepContext.steps.last)
		toFormatEObject.add(newComponentTestStepContext.steps.head)
	}

}
