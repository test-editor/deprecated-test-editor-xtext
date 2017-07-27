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

import com.google.gson.JsonParser
import java.math.BigDecimal
import javax.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmTypeReference
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentVariable
import org.slf4j.LoggerFactory
import org.eclipse.xtext.common.types.JvmEnumerationType

/** 
 * compute whether and how coercion should be generated
 */
class TclCoercionComputer {
	
	private val logger = LoggerFactory.getLogger(TclCoercionComputer)
	
	@Inject extension TclJvmTypeReferenceUtil typeReferenceUtil
	@Inject extension TclJsonUtil

	def initWith(Resource resource) {
		// in order to have access to the classpath (within the reference util) the resource (set) must be set
		typeReferenceUtil.initWith(resource)
	}

	def initWith(ResourceSet resourceSet) {
		// in order to have access to the classpath (within the reference util) the resource (set) must be set
		typeReferenceUtil.initWith(resourceSet)
	}

	def boolean isCoercionPossible(JvmTypeReference targetType, JvmTypeReference sourceType, String value) {
		try {
			switch (targetType) {
				case targetType.isString:
					return true
				case targetType.isLong: {
					Long.valueOf(value) // exception caught by enclosing try
					return true
				}
				case targetType.isBoolean:
					return Boolean.TRUE.toString.equals(value) || Boolean.FALSE.toString.equals(value)
				case targetType.isInt: {
					Integer.valueOf(value) // exception caught by enclosing try
					return true
				}
				case targetType.isJson: {
					new JsonParser().parse(value) // exception caught by enclosing try
					return true
				}
				case targetType.isNumber: {
					java.text.NumberFormat.getInstance().parse(value) // exception caught by enclosing try
					return true
				}
				case targetType.isBigDecimal: {
					new BigDecimal(value) // exception caught by enclosing try
					return true
				}
				case targetType.isEnum: {
					val enumType = (targetType.type as JvmEnumerationType)
					return enumType.literals.exists[simpleName == value]
				}
			}
		} catch (Exception e) {
			logger.trace('''Exception while coercing of value = '«value»' to enum type = '«targetType.qualifiedName»'.''', e)
			return false
		}
		return false
	}
	
	def boolean isCoercionPossible(JvmTypeReference targetType, JvmTypeReference sourceType, StepContent content) {
		val typesAreCoercible = isCoercionPossible(targetType, sourceType)
		if (typesAreCoercible && content instanceof StepContentVariable) {
			val value = (content as StepContentVariable).value
			return isCoercionPossible(targetType, sourceType, value)
		} else {
			return typesAreCoercible
		}
	}
	
	def boolean isCoercionPossible(JvmTypeReference targetType, JvmTypeReference sourceType) {
		switch targetType {
			case targetType.isString : return sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber || sourceType.isBoolean || sourceType.isJson || sourceType.isString || sourceType.isEnum
			case targetType.isLong: return sourceType.isString || sourceType.isJson || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber
			case targetType.isBoolean: return sourceType.isString || sourceType.isJson || sourceType.isBoolean
			case targetType.isInt: return sourceType.isString || sourceType.isJson || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber
			case targetType.isJson: return sourceType.isString || sourceType.isBoolean || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber || sourceType.isJson || sourceType.isEnum
			case targetType.isNumber: return sourceType.isString || sourceType.isJson || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber
			case targetType.isBigDecimal: return sourceType.isString || sourceType.isJson || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber
			case targetType.isEnum: return sourceType.isString || sourceType.isJson || sourceType.isEnum
		}
		return false
	}
	
	def String generateCoercionGuard(JvmTypeReference targetType, JvmTypeReference sourceType, String sourceValue, String quotedErrorMessage) {
		val coercionErrorMessage = '''Coercion not possible from sourceType = '«sourceType?.qualifiedName»' to targetType= '«targetType?.qualifiedName»'.'''
		if (isCoercionPossible(targetType, sourceType)) {
			switch targetType {
				case targetType.isInt:
					if (sourceType.isInt) {
						return ''
					} else if (sourceType.isLong) {
						return '''try { java.lang.Math.toIntExact(«sourceValue»); } catch (ArithmeticException ae) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else if (sourceType.isNumber) {
						return ''
					} else if (sourceType.isJson) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isNumber());'''
					} else if (sourceType.isString) {
						return '''try { Integer.parseInt(«sourceValue»); } catch (NumberFormatException nfe) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else if (sourceType.isBigDecimal) {
						return '''try { «sourceValue».intValueExact(); } catch (ArithmeticException ae) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isLong:
					if (sourceType.isLong || sourceType.isInt || sourceType.isNumber) {
						return ''
					} else if (sourceType.isJson) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isNumber());'''
					} else if (sourceType.isString) {
						return '''try { Long.parseLong(«sourceValue»); } catch (NumberFormatException nfe) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else if (sourceType.isBigDecimal) {
						return '''try { «sourceValue».longValueExact(); } catch (ArithmeticException ae) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
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
				case targetType.isNumber:
					if (sourceType.isJson) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isNumber());'''
					} else if (sourceType.isInt || sourceType.isLong || sourceType.isBigDecimal || sourceType.isNumber || sourceType.isANumber) {
						return ''
					} else if (sourceType.isString) {
						return '''try { java.text.NumberFormat.getInstance().parse(«sourceValue»); } catch (java.text.ParseException pe) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isBigDecimal:
					if (sourceType.isInt || sourceType.isLong || sourceType.isNumber || sourceType.isBigDecimal) {
						return ''
					} else if (sourceType.isJson) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isNumber());'''
					} else if (sourceType.isString) {
						return '''try { new java.math.BigDecimal(«sourceValue»); } catch (NumberFormatException nfe) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isEnum:
					if (sourceType.isString) {
						return '''try { «targetType.qualifiedName».valueOf(«sourceValue»); } catch (IllegalArgumentException ia) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else if (sourceType.isJson) {
						return '''try { «targetType.qualifiedName».valueOf(«sourceValue»«generateJsonElementAccess(stringJvmTypeReference)»); } catch (IllegalArgumentException ia) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else if ((sourceType.isEnum) && isAssignableFrom(targetType, sourceType)) {
						return ''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
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
				case targetType.isNumber:
					if (sourceType.isJson) {
						return '''«sourceValue»«generateJsonElementAccess(targetType)»'''
					} else if (sourceType.isInt || sourceType.isLong || sourceType.isBigDecimal || sourceType.isNumber || sourceType.isANumber) {
						return sourceValue
					} else if (sourceType.isString) {
						return '''java.text.NumberFormat.getInstance().parse(«sourceValue»)'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isJson:
					if (sourceType.isString) {
						return jsonParseInstruction('''"\""+«sourceValue»+"\""''')
					} else if (sourceType.isInt) {
						return jsonParseInstruction('''Integer.toString(«sourceValue»)''')
					} else if (sourceType.isLong) {
						return jsonParseInstruction('''Long.toString(«sourceValue»)''')
					} else if (sourceType.isBoolean) {
						return jsonParseInstruction('''Boolean.toString(«sourceValue»)''')
					} else if (sourceType.isJson) {
						return sourceValue
					} else if (sourceType.isNumber) {
						return jsonParseInstruction('''«sourceValue»''')
					} else if (sourceType.isEnum) {
						return jsonParseInstruction('''"\""+«sourceValue».toString()+"\""''')
					} else {
						return jsonParseInstruction('''«sourceValue».toString()''')
					}
				case targetType.isInt:
					if (sourceType.isString) {
						return '''Integer.parseInt(«sourceValue»)'''
					} else if (sourceType.isLong) {
						return '''java.lang.Math.toIntExact(«sourceValue»)'''
					} else if (sourceType.isNumber) {
						return sourceValue
					} else if (sourceType.isBigDecimal) {
						return '''«sourceValue».intValueExact()'''
					} else if (sourceType.isInt) {
						return sourceValue
					} else if (sourceType.isJson) {
						return '''«sourceValue»«generateJsonElementAccess(targetType)»'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isLong:
					if (sourceType.isString) {
						return '''Long.parseLong(«sourceValue»)'''
					} else if (sourceType.isNumber) {
						return sourceValue
					} else if (sourceType.isLong || sourceType.isInt) {
						return sourceValue
					} else if (sourceType.isBigDecimal) {
						return '''«sourceValue».longValueExact()'''
					} else if (sourceType.isJson) {
						return '''«sourceValue»«generateJsonElementAccess(targetType)»'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isBoolean:
					if (sourceType.isString) {
						return '''Boolean.valueOf(«sourceValue»)'''
					} else if (sourceType.isBoolean) {
						return sourceValue
					} else if (sourceType.isJson) {
						return '''«sourceValue»«generateJsonElementAccess(targetType)»'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isString:
					if (sourceType.isString) {
						return sourceValue
					} else if (sourceType.isInt) {
						return '''Integer.toString(«sourceValue»)'''
					} else if (sourceType.isLong) {
						return '''Long.toString(«sourceValue»)'''
					} else if (sourceType.isBoolean) {
						return '''Boolean.toString(«sourceValue»)'''
					} else if (sourceType.isBigDecimal) {
						return '''«sourceValue».toString()'''
					} else if (sourceType.isNumber) {
						return '''String.valueOf(«sourceValue»)'''
					} else if (sourceType.isJson) {
						return '''«sourceValue»«generateJsonElementAccess(targetType)»'''
					} else if (sourceType.isEnum) {
						return '''«sourceValue».toString()'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isBigDecimal:
					if (sourceType.isBigDecimal) {
						return sourceValue
					} else if (sourceType.isJson) {
						return '''«sourceValue»«generateJsonElementAccess(targetType)»'''
					} else if (sourceType.isInt || sourceType.isLong || sourceType.isString || sourceType.isNumber) {
						return '''new java.math.BigDecimal(«sourceValue»)'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isEnum:
					if (sourceType.isString) {
						return '''«targetType.qualifiedName».valueOf(«sourceValue»)'''
					} else if (sourceType.isJson) {
						return '''«targetType.qualifiedName».valueOf(«sourceValue»«generateJsonElementAccess(stringJvmTypeReference)»)'''
					} else if ((sourceType.isEnum) && isAssignableFrom(targetType, sourceType)) {
						return '''«sourceValue»'''
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
	
	// naive implementation	of check whether the given types can be coerced to something that can be ordered (currently long only)
	def boolean coercableToCommonOrderable(JvmTypeReference typeReferenceA, JvmTypeReference typeReferenceB) {
		// currently only numericals are allowed
		return isCoercionPossible(numberJvmTypeReference, typeReferenceA) &&
			isCoercionPossible(numberJvmTypeReference, typeReferenceB)
	}
	
	def coercedCommonOrderableType(JvmTypeReference typeReferenceA, JvmTypeReference typeReferenceB) {
		if (coercableToCommonOrderable(typeReferenceA, typeReferenceB)) {
			// return numberJvmTypeReference
			if (typeReferenceA.isBigDecimal || typeReferenceB.isBigDecimal || typeReferenceA.isJson || typeReferenceB.isJson) {
				return bigDecimalJvmTypeReference
			} else if (typeReferenceA.isLong || typeReferenceB.isLong) {
				return longObjectJvmTypeReference
			} else if (typeReferenceA.isInt || typeReferenceB.isInt) {
				return intObjectJvmTypeReference
			}
		}
		throw new RuntimeException('''No coercible common orderable type found for typerefA='«typeReferenceA?.qualifiedName»' and typerefB='«typeReferenceB?.qualifiedName»'.''')
	}
	
	def JvmTypeReference coercedCommonComparableType(JvmTypeReference typeReferenceA, JvmTypeReference typeReferenceB) {
		if (typeReferenceA.qualifiedName == typeReferenceB.qualifiedName) {
			return typeReferenceA
		}
		if (isJsonType(typeReferenceA)) {
			return typeReferenceB // JsonType can be coerced to anything (e.g. asJsonPrimitive().asLong())
		}
		if (isJsonType(typeReferenceB)) {
			return typeReferenceA // JsonType can be coerced to anything (e.g. asJsonPrimitive().asLong())
		}
		if (isAssignableFrom(typeReferenceA, typeReferenceB)) { // if assignable, then it is comparable, too
			return typeReferenceA
		}
		if (isAssignableFrom(typeReferenceB, typeReferenceA)) { // if assignable, then it is comparable, too
			return typeReferenceB
		}
		if (isCoercionPossible(typeReferenceA, typeReferenceB)) {
			return typeReferenceA
		}
		if (isCoercionPossible(typeReferenceB, typeReferenceA)) {
			return typeReferenceB
		}
		throw new RuntimeException('''No coercible common comparable type found for typerefA='«typeReferenceA?.qualifiedName»' and typerefB='«typeReferenceB?.qualifiedName»'.''')
	}
	
	// naive implementation of possible coercions or comparable types
	def boolean coercableToCommonComparable(JvmTypeReference typeReferenceA, JvmTypeReference typeReferenceB) {
		return ((typeReferenceA.qualifiedName == typeReferenceA.qualifiedName) ||
			(isJsonType(typeReferenceA)) || (isJsonType(typeReferenceB)) ||
			(isAssignableFrom(typeReferenceA, typeReferenceB)) ||
			(isAssignableFrom(typeReferenceB, typeReferenceA)) ||
			(isCoercionPossible(typeReferenceA, typeReferenceB)) ||
			(isCoercionPossible(typeReferenceB, typeReferenceA)))
	}
	
}
