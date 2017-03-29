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

import javax.inject.Singleton
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.xbase.scoping.XbaseQualifiedNameProvider
import org.testeditor.tcl.TclModel
import com.google.inject.Inject
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

@Singleton
class TclQualifiedNameProvider extends XbaseQualifiedNameProvider {

	@Inject ClasspathUtil classpathUtil

	def QualifiedName qualifiedName(TclModel model) {
		if(model.package==null) {
			model.package = classpathUtil.inferPackage(model)
		}
		return converter.toQualifiedName(model.package)
	}

}
