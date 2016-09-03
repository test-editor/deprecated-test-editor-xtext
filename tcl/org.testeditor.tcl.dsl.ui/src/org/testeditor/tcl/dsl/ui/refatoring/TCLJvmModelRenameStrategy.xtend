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

import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.ui.refactoring.IRefactoringUpdateAcceptor
import org.eclipse.xtext.xbase.ui.jvmmodel.refactoring.DefaultJvmModelRenameStrategy

class TCLJvmModelRenameStrategy extends DefaultJvmModelRenameStrategy {

	override createDeclarationUpdates(String newName, ResourceSet resourceSet,
		IRefactoringUpdateAcceptor updateAcceptor) {
		super.createDeclarationUpdates(newName, resourceSet, updateAcceptor)
		val IPath path = getPathToRename(targetElementOriginalURI, resourceSet);
		if (path != null && path.lastSegment.equals(originalName + ".tcl")) {
			updateAcceptor.accept(targetElementOriginalURI.trimFragment(),
				new RenameResourceChange(path, newName + "." + path.fileExtension))
		}
	}

	def IPath getPathToRename(URI elementURI, ResourceSet resourceSet) {
		val targetObject = resourceSet.getEObject(elementURI, false);
		val resourceURI = EcoreUtil2.getPlatformResourceOrNormalizedURI(targetObject).trimFragment();
		return new Path("/").append(new Path(resourceURI.path()).removeFirstSegments(1));
	}

	
}
