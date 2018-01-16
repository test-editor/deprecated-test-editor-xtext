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
package org.testeditor.tcl.dsl.jvmmodel

import com.google.gson.JsonArray
import com.google.gson.JsonElement
import com.google.gson.JsonNull
import com.google.gson.JsonObject
import com.google.gson.JsonPrimitive
import java.math.BigDecimal
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.tcl.AccessPathElement
import org.testeditor.tcl.ArrayPathElement
import org.testeditor.tcl.KeyPathElement

import static extension org.apache.commons.lang3.StringEscapeUtils.escapeJava

/**
 * Provide a set of functions that hold all com.google.gson specific implementations that
 * are useful for generating access to JsonObject, JsonArray, JsonPrimitive
 */
class TclJsonUtil {
	
	val jsonTypeNames = newHashSet(
		JsonElement.name, 
		JsonObject.name,
		JsonArray.name, 
		JsonNull.name, 
		JsonPrimitive.name
	)
	
	def boolean isJsonType(JvmTypeReference typeReference) {
		val typeRefQNameString = typeReference.qualifiedName
		return jsonTypeNames.contains(typeRefQNameString)
	}
	
	def String generateJsonElementAccess(JvmTypeReference wantedType) {			
			switch wantedType.qualifiedName {
				case BigDecimal.name: return '.getAsJsonPrimitive().getAsBigDecimal()'
				case Number.name:  return '''.getAsJsonPrimitive().getAsNumber()'''
				case Integer.name,
				case int.name: return '''.getAsJsonPrimitive().getAsInt()'''
				case Long.name,
				case long.name: return '''.getAsJsonPrimitive().getAsLong()'''
				case boolean.name,
				case Boolean.name: return '''.getAsJsonPrimitive().getAsBoolean()'''
				case String.name: return '''.getAsJsonPrimitive().getAsString()'''
				default: throw new RuntimeException('''Unknown target type = '«wantedType.qualifiedName»'.''')
			}
	}
	
	def String jsonPathReadAccessToString(AccessPathElement pathElement) {
		switch (pathElement) {
			KeyPathElement: return '''.getAsJsonObject().get("«pathElement.key.escapeJava»")'''
			ArrayPathElement: return '''.getAsJsonArray().get(«pathElement.number.escapeJava»)'''
			default: throw new RuntimeException('''Unknown path element type = '«pathElement.class.name»'.''')
		}
	}
	
	def String jsonPathWriteAccessToString(AccessPathElement pathElement, String value) {
		switch (pathElement) {
			KeyPathElement: return '''.getAsJsonObject().add("«pathElement.key.escapeJava»", «value»)'''
			ArrayPathElement: return '''.getAsJsonArray().set(«pathElement.number.escapeJava», «value»)'''
			default: throw new RuntimeException('''Unknown path element type = '«pathElement.class.name»'.''')
		}
	}
	
	def String jsonParseInstruction(String toParse) {
		return '''new com.google.gson.JsonParser().parse(«toParse»)'''
	}

}