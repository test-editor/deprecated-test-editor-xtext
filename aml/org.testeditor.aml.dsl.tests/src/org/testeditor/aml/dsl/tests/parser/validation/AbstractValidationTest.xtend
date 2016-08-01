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
package org.testeditor.aml.dsl.tests.parser.validation

import org.eclipse.emf.ecore.EObject
import org.testeditor.aml.dsl.tests.parser.AbstractParserTest

abstract class AbstractValidationTest extends AbstractParserTest {
	
	protected def <T extends EObject> T addToResourceSet(T model, String fileName) {
		return model.addToResourceSet(resourceSet, fileName) // only aml model (and children) known here
	}

}
