package org.testeditor.tcl.dsl.ui.editor

import com.google.inject.Inject
import java.util.List
import org.eclipse.emf.common.notify.Notification
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.InternalEObject
import org.eclipse.emf.ecore.impl.ENotificationImpl
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.util.TypeReferences
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.xbase.imports.RewritableImportSection
import org.eclipse.xtext.xtype.XImportDeclaration
import org.eclipse.xtext.xtype.impl.XtypeFactoryImpl
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase

class UpdateTestModelByDropTarget {

	@Inject DropUtils dropUtils
	@Inject RewritableImportSection.Factory rewritableImportSectionFactory
	@Inject TypeReferences references
	@Inject XtypeFactoryImpl xtypeFactory
	@Inject IQualifiedNameProvider qualifiedNameProvider

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

	/**
	 * make sure that wildcard imports are removed so that potential name clashes with simpleName
	 * are prevented
	 */
	private def boolean removeSuspiciousWildcardImports(TclModel tclModel, String simpleName, String qualifiedName) {
		val suspiciousWildcardImports = tclModel.wildcardImports.filter [
			val type = resolveType(tclModel, simpleName)
			return type !== null && type.qualifiedName != qualifiedName // type can be resolved, but resulting qualified name is different
		].toList // explcitily create list to prevent modification exception on (lazy) iterable during the following remove!
		suspiciousWildcardImports.forEach[tclModel.importSection.importDeclarations.remove(it)]
		return !suspiciousWildcardImports.empty
	}

	private def JvmType resolveType(XImportDeclaration wildcardImport, TclModel tclModel, String simpleName) {
		val packageAndDot = wildcardImport.importedNamespace.substring(0, wildcardImport.importedNamespace.length - 1)
		return references.findDeclaredType('''«packageAndDot»«simpleName»''', tclModel)
	}

	private def Iterable<XImportDeclaration> getWildcardImports(TclModel tclModel) {
		if (tclModel.importSection !== null) {
			return tclModel.importSection.importDeclarations.filter [
				importedNamespace !== null && importedNamespace.endsWith("*")
			]
		}
		return emptyList
	}

	/**
	 * make sure that the import section is modified such that no name clashes exist and the 
	 * dropped component context can then reference the given component correctly (preferably via short name,
	 * but fully qualified in case of ambiguities
	 * 
	 * the modification of the import section must take place before actually adding the new
	 * test step component.
	 */
	private def void handleImportSection(XtextResource resource) {
		val tclModel = resource.contents.head as TclModel
		val droppedObject = dropUtils.getDroppedObjectAs(Component)
		val qualifiedName = qualifiedNameProvider.getFullyQualifiedName(droppedObject).toString
		val simpleName = droppedObject.name

		// handle suspicious wildcard imports before actually using the rewritable import section utils
		val wildcardRemovalTookPlace = tclModel.removeSuspiciousWildcardImports(simpleName, qualifiedName)

		val importSection = rewritableImportSectionFactory.parse(resource)
		val clashingImportedTypes = importSection.getImportedTypes(simpleName)
		val importWanted = clashingImportedTypes.nullOrEmpty && !wildcardRemovalTookPlace &&
			!tclModel.wildcardImports.exists[resolveType(tclModel, simpleName)?.qualifiedName == qualifiedName]

		if (importWanted) {
			val added = importSection.addImport(qualifiedName)
			if (!added) { // e.g. this import already exists
				return // no further rewrite of import section necessary
			}
		} else if (!clashingImportedTypes.nullOrEmpty) {
			// remove all imports for the given clashing types (no wildcards)
			clashingImportedTypes.forEach[importSection.removeImport(it)]
		}
		if (tclModel.importSection === null && importWanted) {
			// importSection.update does not automatically create the import section itself
			tclModel.importSection = xtypeFactory.createXImportSection
		}
		importSection.update // makes an update on tclModel
		// make sure that importSection is null (if empty after update) since it may not be empty (syntax rule '+')
		if (tclModel.importSection !== null && tclModel.importSection.importDeclarations.empty) {
			tclModel.importSection = null
		}
		markComponentContextsForQualifiedNameUpdate(tclModel, simpleName)
	}

	/** 
	 * make sure that component contexts are updated with (fully qualified) component names,
	 * if necessary after the modification of the import section 
	 */
	private def void markComponentContextsForQualifiedNameUpdate(TclModel tclModel, String simpleName) {
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
