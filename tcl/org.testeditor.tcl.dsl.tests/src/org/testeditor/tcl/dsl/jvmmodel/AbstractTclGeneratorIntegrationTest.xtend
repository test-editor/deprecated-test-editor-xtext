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

import com.google.common.base.Strings
import com.google.inject.Inject
import org.eclipse.xtext.generator.IGenerator
import org.eclipse.xtext.resource.XtextResourceSet
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

abstract class AbstractTclGeneratorIntegrationTest extends AbstractParserTest {

	static val SPACES_PER_LEVEL = 2

	@Inject protected IGenerator generator

	protected def String generate(TclModel model) {
		generator.doGenerate(model.eResource, fsa)
		val file = getJavaFile(model)
		return file.toString
	}

	protected def XtextResourceSet getResourceSet() {
		return resourceSet
	}

	protected def XtextResourceSet createNewResourceSet() {
		return resourceSetProvider.get
	}

	/**
	 * Lets subclasses define the repeating "header" of the test so that
	 * they don't need to copy&paste it all the time.
	 */
	protected def String getTestHeader() '''
		package com.example
							
		# SimpleTest
	'''

	protected def String parseAndGenerate(CharSequence tcl) {
		val fullTcl = '''
			«testHeader»
			
			«tcl»
		'''
		val tclModel = parseTcl(fullTcl, "SimpleTest.tcl")
		return tclModel.generate
	}

	/**
	 * Indents the passed input to the passed level. Note that the
	 * parameter does not represent the number of spaces but is multiplied
	 * with the {@link #SPACES_PER_LEVEL}.
	 * 
	 * @param input the input
	 * @param level the level of indentation, not the number of spaces
	 */
	protected def String indent(CharSequence input, int level) {
		val indentation = Strings.repeat(' ', SPACES_PER_LEVEL * level)
		return input.toString.replaceAll("(?m)^", indentation)
	}

}
