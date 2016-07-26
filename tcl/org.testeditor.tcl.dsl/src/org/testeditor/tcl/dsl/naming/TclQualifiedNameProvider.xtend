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
package org.testeditor.tcl.dsl.naming

import javax.inject.Inject
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.naming.DefaultDeclarativeQualifiedNameProvider
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.MacroCollection

@Singleton
class TclQualifiedNameProvider extends DefaultDeclarativeQualifiedNameProvider {

	@Inject extension IQualifiedNameConverter

	override getFullyQualifiedName(EObject obj) {
		val result = switch (obj) {
			TclModel:
				obj.package.toQualifiedName
			TestCase:
				(obj.model.package + '.' + obj.name).toQualifiedName
			MacroCollection:
				(obj.model.package + '.' + obj.name).toQualifiedName
			default:
				super.getFullyQualifiedName(obj)
		}
		return result
	}

}
