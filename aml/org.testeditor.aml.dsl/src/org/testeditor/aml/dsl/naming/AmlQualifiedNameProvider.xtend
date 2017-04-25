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
package org.testeditor.aml.dsl.naming

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.xbase.scoping.XbaseQualifiedNameProvider
import org.testeditor.aml.AmlModel
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

class AmlQualifiedNameProvider extends XbaseQualifiedNameProvider {

	@Inject ClasspathUtil classpathUtil

	override getFullyQualifiedName(EObject obj) {
		switch (obj) {
			AmlModel: {
				if (obj.package === null) {
					val derivedPackage = classpathUtil.inferPackage(obj)
					if (derivedPackage.nullOrEmpty) {
						return null
					} else {
						return converter.toQualifiedName(derivedPackage)
					}
				}
				return converter.toQualifiedName(obj.package)
			}				
			default:
				return super.getFullyQualifiedName(obj)
		}
	}

}