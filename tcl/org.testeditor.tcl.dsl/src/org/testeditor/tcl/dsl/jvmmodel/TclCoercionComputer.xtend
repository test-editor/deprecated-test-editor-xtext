package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet

/** compute whether and how coercion should be generated */
class TclCoercionComputer {
	
	@Inject extension TclJvmTypeReferenceUtil tclJvmTypeReferenceUtil
	@Inject TclJsonUtil tclJsonUtil
	
	/** allowed coercions
	 * String <- (long, Long, boolean, Boolean, Json*) 
	 * Long, long <- (String, Json*)
	 * Boolean, boolean <- (String, Json*)
	 */	
	def boolean isCoercionPossible(JvmTypeReference targetType, JvmTypeReference sourceType) {
		switch targetType {
			case targetType.isString : return sourceType.isLong || sourceType.isBoolean || sourceType.isJson
			case targetType.isLong: return sourceType.isString || sourceType.isJson
			case targetType.isBoolean: return sourceType.isString || sourceType.isJson
			case targetType.isJson: return sourceType.isString || sourceType.isBoolean || sourceType.isLong || sourceType.isJson
		}
		return false
	}
	
	def String generateCoercionGuard(JvmTypeReference targetType, JvmTypeReference sourceType, String sourceValue, String quotedErrorMessage) {
		switch targetType {
			case targetType.isLong:
				if (sourceType.isJson) {
					return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isNumber());'''
				} else {
					return '''try { Long.parseLong(«sourceValue»); } catch (NumberFormatException nfe) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
				}
			case targetType.isBoolean:
				if (sourceType.isJson) {
					return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isBoolean());'''
				} else {
					return '''org.junit.Assert.assertTrue(«quotedErrorMessage», Boolean.TRUE.toString().equals(«sourceValue») || Boolean.FALSE.toString().equals(«sourceValue»));'''
				}
			case targetType.isString:
				if (sourceType.isJson) {
					return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isString());'''
				} else {
					return ''
				}
			case targetType.isJson: return ''
			default: throw new RuntimeException('''unknown target type = '«targetType?.qualifiedName»'.''')
		}
	}
	
	def String generateCoercion(JvmTypeReference targetType, JvmTypeReference sourceType, String sourceValue) {
		if (isCoercionPossible(targetType, sourceType)) {
			switch (targetType) {
				case targetType.isJson:
					if (sourceType.isString) {
						return tclJsonUtil.jsonParseInstruction('''"\""+«sourceValue»+"\""''')
					} else if (sourceType.isLong) {
						return tclJsonUtil.jsonParseInstruction('''Long.toString(«sourceValue»)''')
					} else if (sourceType.isBoolean) {
						return tclJsonUtil.jsonParseInstruction('''Boolean.toString(«sourceValue»)''')
					} else if (sourceType.isJson) {
						return sourceValue
					} else {
						return tclJsonUtil.jsonParseInstruction('''«sourceValue».toString()''')
					}
				case targetType.isLong:
					if (sourceType.isJson) {
						return '''«sourceValue».getAsJsonPrimitive().getAsLong()'''
					} else {
						return '''Long.parseLong(«sourceValue»)'''
					}
				case targetType.isBoolean:
					if (sourceType.isJson) {
						return '''«sourceValue».getAsJsonPrimitive().getAsBoolean()'''
					} else {
						return '''Boolean.valueOf(«sourceValue»)'''
					}
				case targetType.isString:
					if (sourceType.isJson) {
						return '''«sourceValue».getAsJsonPrimitive().getAsString()'''
					} else {
						return sourceValue
					}
				default:
					throw new RuntimeException("unknown expected type.")
			}
		}
	}
	
	def initWith(Resource resource) {
		tclJvmTypeReferenceUtil.initWith(resource)
	}

	def initWith(ResourceSet resourceSet) {
		tclJvmTypeReferenceUtil.initWith(resourceSet)
	}
	
	
}