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
package org.testeditor.tsl.dsl.naming

import com.google.inject.Inject
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.naming.DefaultDeclarativeQualifiedNameProvider
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.testeditor.tsl.TslModel

@Singleton
class TslQualifiedNameProvider extends DefaultDeclarativeQualifiedNameProvider {
	
	@Inject extension IQualifiedNameConverter
	
	override getFullyQualifiedName(EObject obj) {
		val result = switch (obj) {
			TslModel:
				obj.package.toQualifiedName
			default:
				super.getFullyQualifiedName(obj)
		}
		return result
	}
	
}