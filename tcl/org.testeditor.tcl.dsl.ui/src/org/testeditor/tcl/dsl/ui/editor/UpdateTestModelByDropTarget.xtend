package org.testeditor.tcl.dsl.ui.editor

import org.eclipse.xtext.resource.XtextResource
import java.util.List
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.ComponentTestStepContext
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.EObject
import org.testeditor.tcl.SpecificationStepImplementation
import org.eclipse.xtext.EcoreUtil2
import org.testeditor.tcl.TestCase
import com.google.inject.Inject
import org.eclipse.xtext.xtype.XImportDeclaration
import org.testeditor.aml.AmlModel

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

			updateTestModel(tclModel.test, dropTarget, newTestStepContext, toFormatEObject)

			toFormat.addAll(toFormatEObject.map[EcoreUtil.getRelativeURIFragmentPath(tclModel, it)])
			currentElement.add(EcoreUtil.getRelativeURIFragmentPath(tclModel, newTestStep))
		}
	}

	public def void updateTestModel(TestCase test, EObject dropTarget, ComponentTestStepContext newTestStepContext,
		List<EObject> toFormatEObject) {

		var testStepIndex = 0
		val ComponentTestStepContext targetTestStepContext = dropUtils.searchTargetTestStepContext(test, dropTarget)

		if (targetTestStepContext === null) {
			insertTargetTestStepContext(test, newTestStepContext, dropTarget, 0, toFormatEObject)
			toFormatEObject.add(newTestStepContext)
		} else {
			testStepIndex = dropUtils.getInsertionIndex(targetTestStepContext, dropTarget)

			if (targetTestStepContext.isNewTestStepContextNeeded(newTestStepContext)) {
				var targetTestStepContextIndex = (targetTestStepContext.eContainer as SpecificationStepImplementation).
					contexts.indexOf(targetTestStepContext)

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
				insertTargetTestStepContext(test, newTestStepContext, dropTarget, targetTestStepContextIndex,
					toFormatEObject)
				toFormatEObject.add(newTestStepContext)
			} else {
				dropUtils.addTestStepToModel(testStepIndex, targetTestStepContext, newTestStepContext.steps.head)
				toFormatEObject.add(targetTestStepContext)
			}
		}

	}

	private def isNewTestStepContextNeeded(ComponentTestStepContext targetTestStepContext,
		ComponentTestStepContext newTestStepContext) {
		return targetTestStepContext.component.name != newTestStepContext.component.name ||
			(targetTestStepContext.component.eContainer as AmlModel).package !=
				(newTestStepContext.component.eContainer as AmlModel).package
	}

	private def void insertTargetTestStepContext(TestCase test, ComponentTestStepContext droppedTestStepContext,
		EObject dropTarget, int contextIndex, List<EObject> toFormatEObject) {

		handleImportSection(test.eContainer as TclModel)

		var SpecificationStepImplementation specification = null
		if (test.steps.size() == 0) {
			specification = dropUtils.createSpecification
			toFormatEObject.add(specification)
			test.steps.add(specification)
		} else if (test.steps.last.contexts.size() == 0) {
			specification = test.steps.last
			toFormatEObject.add(specification)
		} else {
			if (dropTarget === null) {
				specification = test.steps.last
				toFormatEObject.add(specification.contexts.last)
			} else {
				specification = EcoreUtil2.getContainerOfType(dropTarget, SpecificationStepImplementation) ?:
					test.steps.head
			}
		}
		specification.contexts.add(contextIndex, droppedTestStepContext)
	}

	private def handleImportSection(TclModel tclModel) {
		// TODO: refactoring needed after the import section was fixed
		val XImportDeclaration newImportDeclaration = dropUtils.createXImportDeclaration => [
			it.importedNamespace = dropUtils.getDroppedObjectAs(String) + ".*"
		]

		if (tclModel.hasImportFor(newImportDeclaration.importedNamespace)) {
			if (tclModel.importSection === null) {
				tclModel.importSection = dropUtils.createXImportSection
			}
			tclModel.importSection.importDeclarations.add(newImportDeclaration)
		}
	}

	def hasImportFor(TclModel tclModel, String namespace) {
		// TODO: refactoring needed after the import section was fixed
		return dropUtils.getDroppedObjectAs(String) != tclModel.package && (tclModel.importSection === null ||
			!tclModel.importSection.importDeclarations.exists[it.importedNamespace == namespace])
	}

	private def splitedTargetTestStepContext(ComponentTestStepContext targetTestStepContext,
		int targetTestStepContextIndex, int insertionIndex, List<EObject> toFormatEObject) {

		val newComponentTestStepContext = dropUtils.createComponentTestStepContext
		newComponentTestStepContext.component = targetTestStepContext.component
		val specification = EcoreUtil2.getContainerOfType(targetTestStepContext, SpecificationStepImplementation)

		specification.contexts.add(targetTestStepContextIndex + 1, newComponentTestStepContext)

		val stepsBeingMoved = targetTestStepContext.steps.subList(insertionIndex, targetTestStepContext.steps.size())
		newComponentTestStepContext.steps.addAll(stepsBeingMoved)

		toFormatEObject.add(targetTestStepContext.steps.last)
		toFormatEObject.add(newComponentTestStepContext.steps.head)
	}

}
