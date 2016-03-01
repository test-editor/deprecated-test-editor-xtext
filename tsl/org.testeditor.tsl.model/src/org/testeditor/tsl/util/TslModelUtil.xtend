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
package org.testeditor.tsl.util

import java.util.List
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentVariable

@Singleton
class TslModelUtil {

	def String getExpectedName(EObject model) {
		val lastSegment = model.eResource?.URI?.lastSegment
		if (lastSegment !== null) {
			val separator = lastSegment.lastIndexOf('.')
			if (separator >= 0) {
				return lastSegment.substring(0, separator).toFirstUpper
			} else {
				return lastSegment.toFirstUpper
			}
		}
		return null
	}

	def String restoreString(List<StepContent> contents) {
		return contents.map [
			switch (it) {
				StepContentVariable: '''"«value»"'''
				default: value
			}
		].join(' ')
	}
	
	def boolean matches(SpecificationStep first, SpecificationStep second) {
		return first.contents.restoreString == second.contents.restoreString
	}
	
}