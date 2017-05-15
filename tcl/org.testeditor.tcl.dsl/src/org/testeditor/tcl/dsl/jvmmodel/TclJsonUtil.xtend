/*******************************************************************************
 * Copyright (c) 2012 - 2017 Signal Iduna Corporation and others.
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

import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.tcl.AccessPathElement
import org.testeditor.tcl.KeyPathElement
import org.testeditor.tcl.ArrayPathElement

/**
 * Provide a set of functions that hold all com.google.gson specific implementations that
 * are useful for generating access to JsonObject, JsonArray, JsonPrimitive
 */
class TclJsonUtil {
	
	val jsonTypeNames = newHashSet(
		com.google.gson.JsonElement.name, 
		com.google.gson.JsonObject.name,
		com.google.gson.JsonArray.name, 
		com.google.gson.JsonNull.name, 
		com.google.gson.JsonPrimitive.name
	)
	
	def boolean isJsonType(JvmTypeReference typeReference) {
		val typeRefQNameString = typeReference.qualifiedName
		return jsonTypeNames.contains(typeRefQNameString)
	}
	
	def String generateJsonElementAccess(JvmTypeReference wantedType) {			
			switch wantedType.qualifiedName {
				case Integer.name,
				case int.name: return '''.getAsJsonPrimitive().getAsInt()'''
				case Long.name,
				case long.name: return '''.getAsJsonPrimitive().getAsLong()'''
				case boolean.name,
				case Boolean.name: return '''.getAsJsonPrimitive().getAsBoolean()'''
				case String.name: return '''.getAsJsonPrimitive().getAsString()'''
			}
	}
	
	def String jsonPathReadAccessToString(AccessPathElement pathElement) {
		switch (pathElement) {
			KeyPathElement: return '''.getAsJsonObject().get("«pathElement.key»")'''
			ArrayPathElement: return '''.getAsJsonArray().get(«pathElement.number»)'''
			default: throw new RuntimeException('''Unknown path element type = '«pathElement.class.name»'.''')
		}
	}
	
	def String jsonPathWriteAccessToString(AccessPathElement pathElement, String value) {
		switch (pathElement) {
			KeyPathElement: return '''.getAsJsonObject().add("«pathElement.key»", «value»)'''
			ArrayPathElement: return '''.getAsJsonArray().set(«pathElement.number», «value»)'''
			default: throw new RuntimeException('''Unknown path element type = '«pathElement.class.name»'.''')
		}
	}
	
	def String jsonParseInstruction(String toParse) {
		return '''new com.google.gson.JsonParser().parse(«toParse»)'''
	}

}