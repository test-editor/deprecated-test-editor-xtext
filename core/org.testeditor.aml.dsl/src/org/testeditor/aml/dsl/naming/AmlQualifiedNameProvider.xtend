/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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
package org.testeditor.aml.dsl.naming

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.SimpleNameProvider
import org.testeditor.aml.model.InteractionType
import org.testeditor.aml.model.TemplateVariable

class AmlQualifiedNameProvider extends SimpleNameProvider {

	@Inject
	extension IQualifiedNameConverter

	override getFullyQualifiedName(EObject obj) {
		if (obj instanceof TemplateVariable) {
			return getFullyQualifiedNameFor(obj)
		}
		return super.getFullyQualifiedName(obj)
	}

	protected def getFullyQualifiedNameFor(TemplateVariable variable) {
		if (variable.name == "element") {
			return null // shall not be referenced from the outside world
		}
		val container = variable.eContainer?.eContainer
		if (container instanceof InteractionType) {
			return '''«container.name».«variable.name»'''.toString.toQualifiedName
		}
		return null
	}

}