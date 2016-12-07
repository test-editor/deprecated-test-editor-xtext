package org.testeditor.tcl.dsl.ui.editor

import com.google.inject.Inject
import java.util.List
import org.eclipse.emf.common.notify.Notification
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.InternalEObject
import org.eclipse.emf.ecore.impl.ENotificationImpl
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.util.TypeReferences
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.xbase.imports.RewritableImportSection
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase

class UpdateTestModelByDropTarget {

	@Inject DropUtils dropUtils
	@Inject RewritableImportSection.Factory importSectionFactory
	@Inject TypeReferences references

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

	protected def updateImports(XtextResource resource, DropTargetXtextEditor editor, List<String> currentElement) {
		handleImportSection(resource)
		return resource
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

	@Inject IQualifiedNameProvider qualifiedNameProvider
	
	private def boolean removeSuspiciousWildcardImports(TclModel tclModel, String simpleName){
		if (tclModel.importSection !== null) {
			val wildcardImports = tclModel.importSection.importDeclarations.filter [
				importedNamespace !== null && importedNamespace.endsWith("*")
			]
			val suspiciousWildcardImports = wildcardImports.filter [
				references.findDeclaredType(importedNamespace.substring(0, importedNamespace.length - 1) + simpleName,
					tclModel) !== null
			].toList // toList to prevent modification exception on (lazy) iterable!
			suspiciousWildcardImports.forEach[tclModel.importSection.importDeclarations.remove(it)]
			return !suspiciousWildcardImports.empty
		}
		return false
	}

	private def void handleImportSection(XtextResource resource) {
		val tclModel = resource.contents.head as TclModel
		val droppedObject = dropUtils.getDroppedObjectAs(Component)
		val qualifiedName = qualifiedNameProvider.getFullyQualifiedName(droppedObject).toString
		val simpleName = droppedObject.name

		val wildcardRemoveTookPlace = removeSuspiciousWildcardImports(tclModel, simpleName)
		val importSection = importSectionFactory.parse(resource)

		val clashingImportedTypes = importSection.getImportedTypes(simpleName)
		if (clashingImportedTypes.nullOrEmpty && !wildcardRemoveTookPlace) {
			val added = importSection.addImport(qualifiedName)
			if (!added) {
				return // no further rewrite of import section necessary
			}
		} else if (!clashingImportedTypes.nullOrEmpty) {
			clashingImportedTypes.forEach[importSection.removeImport(it)]
		}
		importSection.update
		// make sure that importSection is null since it may not be empty (syntax rule '+')
		if (tclModel.importSection !== null && tclModel.importSection.importDeclarations.empty) {
			tclModel.importSection = null
		}
		markComponentsForUpdate(tclModel, simpleName)
	}
	
	private def void markComponentsForUpdate(TclModel tclModel, String simpleName) {
		// make sure that clashing components are updated (with full qualified name)
		val clashingContexts = tclModel.test.steps.map[contexts].flatten.filter(ComponentTestStepContext).filter [
			component.name == simpleName
		]
		clashingContexts.forEach [
			eNotify( // notification of a relevant change (which never really took place)
				new ENotificationImpl(it as InternalEObject, Notification.REMOVE,
					TclPackage.COMPONENT_TEST_STEP_CONTEXT__STEPS, null, null, 0))
		]
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
