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

import java.util.Set
import javax.inject.Inject
import javax.inject.Named
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.ui.refactoring.IRefactoringUpdateAcceptor
import org.eclipse.xtext.xbase.ui.jvmmodel.refactoring.DefaultJvmModelRenameStrategy

import static org.eclipse.xtext.Constants.*

class TCLJvmModelRenameStrategy extends DefaultJvmModelRenameStrategy {

	val Set<String> fileExtensions

	@Inject
	new(@Named(FILE_EXTENSIONS) String fileExtensions) {
		this.fileExtensions = fileExtensions.split(",").map[toLowerCase].toSet
	}

	override createDeclarationUpdates(String newName, ResourceSet resourceSet,
		IRefactoringUpdateAcceptor updateAcceptor) {
		super.createDeclarationUpdates(newName, resourceSet, updateAcceptor)
		val IPath path = getPathToRename(targetElementOriginalURI, resourceSet);
		if (path != null) {
			val fileExtension = path.getFileExtension
			if (fileExtensions.contains(fileExtension) && path.lastSegment.equals(originalName + "." + fileExtension)) {
				updateAcceptor.accept(targetElementOriginalURI.trimFragment(),
					new RenameResourceChange(path, newName + "." + path.fileExtension))
			}
		}
	}

	def private IPath getPathToRename(URI elementURI, ResourceSet resourceSet) {
		val targetObject = resourceSet.getEObject(elementURI, false);
		val resourceURI = EcoreUtil2.getPlatformResourceOrNormalizedURI(targetObject).trimFragment();
		return new Path("/").append(new Path(resourceURI.path()).removeFirstSegments(1));
	}

}
