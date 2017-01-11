/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
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
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.StepContainer
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestStepContext

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
		updateTestModel(tclModel, dropTarget, droppedTestStepContext, eObjectsToFormat)

		// transform eObjects to paths in order to find them in the transformed model (after several editor modifications => reparse)
		eObjectPathsToFormat.addAll(eObjectsToFormat.map[EcoreUtil.getRelativeURIFragmentPath(tclModel, it)])
		// tclModel is ancestor after insertion => get path after insertion
		insertedTestStepPath.set(EcoreUtil.getRelativeURIFragmentPath(tclModel, stepToInsert))
	}

	public def void updateTestModel(TclModel tclModel, EObject dropTarget, ComponentTestStepContext newTestStepContext,
		List<EObject> eObjectsToFormat) {

		var testStepIndex = 0

		val TestStepContext targetTestStepContext = dropUtils.searchTargetTestStepContext(tclModel, dropTarget)

		if (targetTestStepContext === null) {
			insertTargetTestStepContext(tclModel, newTestStepContext, dropTarget, -1, eObjectsToFormat)
			eObjectsToFormat.add(newTestStepContext)
		} else {
			testStepIndex = dropUtils.getInsertionIndex(targetTestStepContext, dropTarget)

			if (targetTestStepContext.isNewTestStepContextNeeded(newTestStepContext)) {
				var targetTestStepContextIndex = (targetTestStepContext.eContainer as StepContainer).contexts.indexOf(
					targetTestStepContext)

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
				insertTargetTestStepContext(tclModel, newTestStepContext, dropTarget, targetTestStepContextIndex,
					eObjectsToFormat)
				eObjectsToFormat.add(newTestStepContext)
			} else {
				dropUtils.addTestStepToModel(testStepIndex, targetTestStepContext, newTestStepContext.steps.head)
				eObjectsToFormat.add(targetTestStepContext)
			}

		}
	}

	private def isNewTestStepContextNeeded(TestStepContext targetTestStepContext,
		ComponentTestStepContext newTestStepContext) {
		if (targetTestStepContext instanceof ComponentTestStepContext) {
			return targetTestStepContext.component.name != newTestStepContext.component.name ||
				(targetTestStepContext.component.eContainer as AmlModel).package !=
					(newTestStepContext.component.eContainer as AmlModel).package
		}
		return true
	}

	private def void insertTargetTestStepContext(TclModel tclModel, ComponentTestStepContext droppedTestStepContext,
		EObject dropTarget, int contextIndex, List<EObject> toFormatEObject) {

		var StepContainer specification = null
		if (tclModel.test != null) {
			if (tclModel.test.steps.empty) {
				tclModel.test.steps.add(dropUtils.createSpecification)
			}
			specification = EcoreUtil2.getContainerOfType(dropTarget, StepContainer) ?: tclModel.test.steps.last
		}
		if (tclModel.macroCollection != null) {

			if (tclModel.macroCollection.macros.empty) {
				tclModel.macroCollection.macros.add(dropUtils.createMacro)
			}
			specification = EcoreUtil2.getContainerOfType(dropTarget, StepContainer) ?:
				tclModel.macroCollection.macros.last
		}

		if (specification.contexts.empty) {
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
		val contexts = tclModel.test?.steps?.map[contexts]?:#[] + tclModel.macroCollection?.macros?.map[contexts]?:#[]
		val clashingContexts = contexts.filterNull.flatten.filter(ComponentTestStepContext).filter [
			component.name == simpleName
		]
		clashingContexts.forEach [
			eNotify( // notification of a relevant change (which never really took place)
			new ENotificationImpl(it as InternalEObject, Notification.REMOVE,
				TclPackage.COMPONENT_TEST_STEP_CONTEXT__STEPS, null, null, 0))
		]
	}

	private def splitedTargetTestStepContext(TestStepContext targetTestStepContext, int targetTestStepContextIndex,
		int insertionIndex, List<EObject> toFormatEObject) {

		var TestStepContext newTestStepContext = null
		if (targetTestStepContext instanceof ComponentTestStepContext) {
			newTestStepContext = dropUtils.createComponentTestStepContext;
			(newTestStepContext as ComponentTestStepContext).component = targetTestStepContext.component
		}
		if (targetTestStepContext instanceof MacroTestStepContext) {
			newTestStepContext = dropUtils.createMacroTestStepContext;
			(newTestStepContext as MacroTestStepContext).macroCollection = targetTestStepContext.macroCollection
		}
		val specification = EcoreUtil2.getContainerOfType(targetTestStepContext, StepContainer)

		specification.contexts.add(targetTestStepContextIndex + 1, newTestStepContext)

		val stepsBeingMoved = targetTestStepContext.steps.subList(insertionIndex, targetTestStepContext.steps.size())
		newTestStepContext.steps.addAll(stepsBeingMoved)

		toFormatEObject.add(targetTestStepContext.steps.last)
		toFormatEObject.add(newTestStepContext.steps.head)
	}

}
