package org.testeditor.tcl.dsl.ui.editor

import com.google.inject.Inject
import java.util.List
import java.util.concurrent.atomic.AtomicReference
import org.eclipse.emf.common.notify.Notification
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.InternalEObject
import org.eclipse.emf.ecore.impl.ENotificationImpl
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.util.TypeReferences
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.xbase.imports.RewritableImportSection
import org.eclipse.xtext.xtype.XImportDeclaration
import org.eclipse.xtext.xtype.XtypeFactory
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase

class TclModelDragAndDropUpdater {


	@Inject DropUtils dropUtils
	@Inject RewritableImportSection.Factory rewritableImportSectionFactory
	@Inject TypeReferences references
	@Inject XtypeFactory xtypeFactory

	def void updateModel(TclModel tclModel, EObject dropTarget, ComponentTestStepContext droppedTestStepContext,
		List<String> eObjectPathsToFormat, AtomicReference<String> insertedTestStepPath) {

		val eObjectsToFormat = newArrayList
		val stepToInsert = droppedTestStepContext.steps.head

		// hereafter newTestStepContex.steps may be cleared, since a context may be reused
		updateTestModel(tclModel.test, dropTarget, droppedTestStepContext, eObjectsToFormat)

		// transform eObjects to paths in order to find them in the transformed model (after several editor modifications => reparse)
		eObjectPathsToFormat.addAll(eObjectsToFormat.map[EcoreUtil.getRelativeURIFragmentPath(tclModel, it)])
		// tclModel is ancestor after insertion => get path after insertion
		insertedTestStepPath.set(EcoreUtil.getRelativeURIFragmentPath(tclModel, stepToInsert))
	}

	public def void updateTestModel(TestCase test, EObject dropTarget, ComponentTestStepContext newTestStepContext,
		List<EObject> eObjectsToFormat) {

		var testStepIndex = 0
		val ComponentTestStepContext targetTestStepContext = dropUtils.searchTargetTestStepContext(test, dropTarget)

		if (targetTestStepContext === null) {
			insertTargetTestStepContext(test, newTestStepContext, dropTarget, 0, eObjectsToFormat)
			eObjectsToFormat.add(newTestStepContext)
		} else {
			testStepIndex = dropUtils.getInsertionIndex(targetTestStepContext, dropTarget)

			if (targetTestStepContext.isNewTestStepContextNeeded(newTestStepContext)) {
				var targetTestStepContextIndex = (targetTestStepContext.eContainer as SpecificationStepImplementation).
					contexts.indexOf(targetTestStepContext)

				// Insert in the middle of an existing TestStepContext
				if (testStepIndex > 0 && testStepIndex < targetTestStepContext.steps.size()) {
					splitedTargetTestStepContext(targetTestStepContext, targetTestStepContextIndex, testStepIndex,
						eObjectsToFormat)
				}
				// If it is not dropped at the top, insert new TestSepContext after the existing TestStepContext
				if (testStepIndex > 0) {
					targetTestStepContextIndex++
					eObjectsToFormat.add(targetTestStepContext.steps.last)
				}
				insertTargetTestStepContext(test, newTestStepContext, dropTarget, targetTestStepContextIndex,
					eObjectsToFormat)
				eObjectsToFormat.add(newTestStepContext)
			} else {
				dropUtils.addTestStepToModel(testStepIndex, targetTestStepContext, newTestStepContext.steps.head)
				eObjectsToFormat.add(targetTestStepContext)
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
	 * make sure that all wildcard imports are removed that have name clashes with simpleName
	 */
	private def boolean removeSuspiciousWildcardImports(TclModel tclModel, String simpleName, String qualifiedName) {
		val suspiciousWildcardImports = tclModel.wildcardImports.filter [
			val type = resolveType(tclModel, simpleName)
			return type !== null && type.qualifiedName != qualifiedName // type can be resolved, but resulting qualified name is different
		].toList // explicitly create list to prevent modification exception on (lazy) iterable during the following remove!
		suspiciousWildcardImports.forEach[tclModel.importSection.importDeclarations.remove(it)]
		return !suspiciousWildcardImports.empty
	}

	/**
	 * (try) to resolve the simple name to a type using the given wildcardImport (to complete the simple name) within the given tclModel.
	 * return null if the type cannot be resolved.
	 */
	private def JvmType resolveType(XImportDeclaration wildcardImport, TclModel tclModel, String simpleName) {
		val packageAndDot = wildcardImport.importedNamespace.substring(0, wildcardImport.importedNamespace.length - 1)
		return references.findDeclaredType('''«packageAndDot»«simpleName»''', tclModel)
	}

	private def Iterable<XImportDeclaration> getWildcardImports(TclModel tclModel) {
		if (tclModel.importSection !== null) {
			return tclModel.importSection.importDeclarations.filter [
				importedNamespace !== null && importedNamespace.endsWith("*")
			]
		} else {
			return emptyList
		}
	}

	/**
	 * make sure that the import section is modified such that no name clashes exist and the 
	 * dropped component context can then reference the given component correctly (preferably via short name,
	 * but fully qualified in case of ambiguities
	 * 
	 * the modification of the import section must take place before actually adding the new
	 * test step component.
	 */
	public def void updateImports(TclModel tclModel, Component droppedObject, String qualifiedName) {
		val simpleName = droppedObject.name

		// handle suspicious wildcard imports before actually using the rewritable import section utils, since they do not handle wildcard imports
		val wildcardRemovalTookPlace = tclModel.removeSuspiciousWildcardImports(simpleName, qualifiedName)

		val importSection = rewritableImportSectionFactory.parse(tclModel.eResource as XtextResource)
		val clashingImportedTypes = importSection.getImportedTypes(simpleName)?.filter [
			it.qualifiedName != qualifiedName // simple name is the same, but qualified name differs
		]
		val droppedAlreadyImportedViaWildcard = tclModel.wildcardImports.exists [
			resolveType(tclModel, simpleName)?.qualifiedName == qualifiedName
		]
		val importWanted = clashingImportedTypes.nullOrEmpty && !wildcardRemovalTookPlace &&
			!droppedAlreadyImportedViaWildcard

		if (importWanted) {
			val added = importSection.addImport(qualifiedName)
			if (!added) { // e.g. this import already exists
				return // no further rewrite of import section necessary
			}
		}
		// remove all imports for the given clashing types (no wildcards)
		clashingImportedTypes?.forEach[importSection.removeImport(it)]
		importSection.saveUpdate(tclModel)
		markComponentContextsForQualifiedNameUpdate(tclModel, simpleName)
	}

	/**
	 * make a safe update of the import section of this tcl model and ensure that 
	 * the tclModel.importSection is null, if no imports remain 
	 */
	private def void saveUpdate(RewritableImportSection importSection, TclModel tclModel) {
		if (tclModel.importSection === null) {
			// importSection.update does not automatically create the import section itself
			tclModel.importSection = xtypeFactory.createXImportSection
		}
		importSection.update // makes an update on tclModel (additions works only if the importSection is not null)
		// make sure that importSection is null (if empty after update) since it may not be empty (xtypes syntax rule '+')
		if (tclModel.importSection !== null && tclModel.importSection.importDeclarations.empty) {
			tclModel.importSection = null
		}
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