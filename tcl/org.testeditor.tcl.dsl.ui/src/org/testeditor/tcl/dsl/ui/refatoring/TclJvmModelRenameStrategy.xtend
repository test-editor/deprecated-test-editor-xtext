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
package org.testeditor.tcl.dsl.ui.refatoring

import com.google.common.annotations.VisibleForTesting
import java.util.Set
import javax.inject.Inject
import javax.inject.Named
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange
import org.eclipse.xtext.ui.refactoring.IRefactoringUpdateAcceptor
import org.eclipse.xtext.xbase.ui.jvmmodel.refactoring.DefaultJvmModelRenameStrategy
import org.testeditor.tcl.TclModel

import static org.eclipse.xtext.Constants.*

class TclJvmModelRenameStrategy extends DefaultJvmModelRenameStrategy {

	val Set<String> fileExtensions

	@Inject
	new(@Named(FILE_EXTENSIONS) String fileExtensions) {
		this.fileExtensions = fileExtensions.split(",").map[toLowerCase].toSet
	}

	override createDeclarationUpdates(String newName, ResourceSet resourceSet, IRefactoringUpdateAcceptor updateAcceptor) {
		super.createDeclarationUpdates(newName, resourceSet, updateAcceptor)
		val path = getPathToRename(targetElementOriginalURI, resourceSet)
		if (path !== null) {
			val fileExtension = path.fileExtension
			val isFileToRename = fileExtensions.contains(fileExtension.toLowerCase) && path.lastSegment == originalName + '.' + fileExtension
			if (isFileToRename) {
				val change = new RenameResourceChange(path, newName + '.' + fileExtension)
				updateAcceptor.accept(targetElementOriginalURI.trimFragment, change)
			}
		}
	}

	/**
	 * The element being renamed has an URI such as {@code platform:/resource/example/src/test/java/example/Test.tcl#/0/@test}.
	 * We need to create an IPath such as {@code /example/src/test/java/example/Test.tcl} from it.
	 */
	@VisibleForTesting
	protected def IPath getPathToRename(URI elementURI, ResourceSet resourceSet) {
		val targetObject = resourceSet.getEObject(elementURI, false)
		val isTopLevelElement = targetObject.eContainer instanceof TclModel
		if (isTopLevelElement) {
			// we should only do this if the element is a top level element
			// (we could have other named elements as children => don't want to rename the resource if they're renamed
			val resourceURI = elementURI.trimFragment
			return new Path("/").append(new Path(resourceURI.path).removeFirstSegments(1))
		}
		return null
	}

}
