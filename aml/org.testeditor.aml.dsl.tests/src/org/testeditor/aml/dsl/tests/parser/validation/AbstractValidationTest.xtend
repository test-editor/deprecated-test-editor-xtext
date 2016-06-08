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

import java.util.UUID
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.testeditor.aml.dsl.tests.parser.AbstractParserTest

abstract class AbstractValidationTest extends AbstractParserTest {
	
	protected def <T extends EObject> T register(T model, String fileExtension) {
		val uri = URI.createURI(UUID.randomUUID.toString + "." + fileExtension)

		val newResource = resourceSet.createResource(uri)
		newResource.contents.add(model)
		return model
	}

}
