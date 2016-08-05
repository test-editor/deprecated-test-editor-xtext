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
package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.xtext.generator.IGenerator
import org.eclipse.xtext.resource.XtextResourceSet
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

abstract class AbstractTclGeneratorIntegrationTest extends AbstractParserTest {

	@Inject protected IGenerator generator

	protected def String generate(TclModel model) {
		generator.doGenerate(model.eResource, fsa)
		val file = getJavaFile(model)
		return file.toString
	}
	
	protected def XtextResourceSet getResourceSet() {
		return 	resourceSet
	}
	
	protected def XtextResourceSet createNewResourceSet() {
		return resourceSetProvider.get
	}

}
