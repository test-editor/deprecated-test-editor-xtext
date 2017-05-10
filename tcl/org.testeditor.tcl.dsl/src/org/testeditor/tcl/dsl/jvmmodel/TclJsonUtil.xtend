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
	
	def boolean isJsonType(JvmTypeReference typeReference) {
		val typeRefQNameString = typeReference.qualifiedName
		switch typeRefQNameString {
			case com.google.gson.JsonElement.name,
			case com.google.gson.JsonObject.name,
			case com.google.gson.JsonArray.name,
			case com.google.gson.JsonPrimitive.name: return true
			default: return false
		}
	}
	
	def String generateJsonElementAccess(JvmTypeReference wantedType) {			
			switch wantedType.qualifiedName {
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