/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
package org.testeditor.xmllibrary.domain.util

import java.io.File
import java.io.StringReader
import java.net.URL
import javax.inject.Singleton
import javax.xml.bind.JAXBContext
import javax.xml.bind.Unmarshaller
import org.testeditor.xmllibrary.domain.action.ActionGroups
import org.testeditor.xmllibrary.domain.binding.TechnicalBindingTypes

/**
 * Provides helper methods for unmarshalling {@link ActionGroups}
 * and {@link TechnicalBindingTypes}.
 */
@Singleton
class XmlHelper {

	val context = JAXBContext.newInstance(ActionGroups, TechnicalBindingTypes)
	
	def Unmarshaller createUnmarshaller() {
		return context.createUnmarshaller
	}
	
	def ActionGroups unmarshalActionGroups(File file) {
		return createUnmarshaller.unmarshal(file) as ActionGroups 
	}
	
	def ActionGroups unmarshalActionGroups(URL url) {
		return createUnmarshaller.unmarshal(url) as ActionGroups 
	}
	
	def ActionGroups unmarshalActionGroups(CharSequence charSequence) {
		return createUnmarshaller.unmarshal(new StringReader(charSequence.toString)) as ActionGroups 
	}
	
	def TechnicalBindingTypes unmarshalTechnicalBindings(File file) {
		return createUnmarshaller.unmarshal(file) as TechnicalBindingTypes 
	}
	
	def TechnicalBindingTypes unmarshalTechnicalBindings(URL url) {
		return createUnmarshaller.unmarshal(url) as TechnicalBindingTypes 
	}
	
	def TechnicalBindingTypes unmarshalTechnicalBindings(CharSequence charSequence) {
		return createUnmarshaller.unmarshal(new StringReader(charSequence.toString)) as TechnicalBindingTypes 
	}
	
}