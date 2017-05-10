package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmTypeReference

/** compute whether and how coercion should be generated */
class TclCoercionComputer {
	
	@Inject extension TclJvmTypeReferenceUtil tclJvmTypeReferenceUtil
	@Inject TclJsonUtil tclJsonUtil
	
	def boolean isCoercionPossible(JvmTypeReference targetType, JvmTypeReference sourceType) {
		switch targetType {
			case targetType.isString : return sourceType.isLong || sourceType.isBoolean || sourceType.isJson || sourceType.isString
			case targetType.isLong: return sourceType.isString || sourceType.isJson || sourceType.isLong
			case targetType.isBoolean: return sourceType.isString || sourceType.isJson || sourceType.isBoolean
			case targetType.isJson: return sourceType.isString || sourceType.isBoolean || sourceType.isLong || sourceType.isJson
		}
		return false
	}
	
	def String generateCoercionGuard(JvmTypeReference targetType, JvmTypeReference sourceType, String sourceValue, String quotedErrorMessage) {
		val coercionErrorMessage = '''Coercion not possible from sourceType = '«sourceType?.qualifiedName»' to targetType= '«targetType?.qualifiedName»'.'''
		if (isCoercionPossible(targetType, sourceType)) {
			switch targetType {
				case targetType.isLong:
					if (sourceType.isLong) {
						return ''
					} else if (sourceType.isJson) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isNumber());'''
					} else if (sourceType.isString) {
						return '''try { Long.parseLong(«sourceValue»); } catch (NumberFormatException nfe) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isBoolean:
					if (sourceType.isBoolean) {
						return ''
					} else if (sourceType.isJson) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isBoolean());'''
					} else if (sourceType.isString) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», Boolean.TRUE.toString().equals(«sourceValue») || Boolean.FALSE.toString().equals(«sourceValue»));'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isString:
					if (sourceType.isJson) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isString());'''
					} else { // long, bool etc. need no guard, since they can all be converted to string
						return ''
					}
				case targetType.isJson: return '' // no guard for json, since that is parsed (and checked) by json library
				default: throw new RuntimeException('''Unknown target type = '«targetType?.qualifiedName»'.''')
			}		
		} else {
			throw new RuntimeException(coercionErrorMessage)
		}
	}
	
	def String generateCoercion(JvmTypeReference targetType, JvmTypeReference sourceType, String sourceValue) {
		val coercionErrorMessage = '''Coercion not possible from sourceType = '«sourceType?.qualifiedName»' to targetType= '«targetType?.qualifiedName»'.'''
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
					if (sourceType.isString) {
						return '''Long.parseLong(«sourceValue»)'''
					} else if (sourceType.isLong) {
						return sourceValue
					} else if (sourceType.isJson) {
						return '''«sourceValue»«tclJsonUtil.generateJsonElementAccess(targetType)»'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isBoolean:
					if (sourceType.isString) {
						return '''Boolean.valueOf(«sourceValue»)'''
					} else if (sourceType.isBoolean) {
						return sourceValue
					} else if (sourceType.isJson) {
						return '''«sourceValue»«tclJsonUtil.generateJsonElementAccess(targetType)»'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isString:
					if (sourceType.isString) {
						return sourceValue
					} else if (sourceType.isLong) {
						return '''Long.toString(«sourceValue»)'''
					} else if (sourceType.isBoolean) {
						return '''Boolean.toString(«sourceValue»)'''
					} else if (sourceType.isJson) {
						return '''«sourceValue»«tclJsonUtil.generateJsonElementAccess(targetType)»'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				default:
					throw new RuntimeException('''Unknown/unsupported targetType = '«targetType?.qualifiedName»' for coercion guard.''')
			}
		} else {
			throw new RuntimeException(coercionErrorMessage)
		}
	}
	
	def initWith(Resource resource) {
		tclJvmTypeReferenceUtil.initWith(resource)
	}

	def initWith(ResourceSet resourceSet) {
		tclJvmTypeReferenceUtil.initWith(resourceSet)
	}
	
	
}