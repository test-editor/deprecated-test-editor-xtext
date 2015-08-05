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
package org.testeditor.aml.dsl.generator

import org.testeditor.aml.model.ModelElement

class AbstractGenerator {
	
	/**
	 * If the label is not set we'd like to use the name as label
	 * in the generation step.
	 */
	protected def String labelOrName(ModelElement element) {
		if (!element.label.nullOrEmpty) {
			return element.label
		} else {
			return element.name
		}
	}
	
}