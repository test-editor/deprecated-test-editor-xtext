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

import com.google.common.annotations.VisibleForTesting
import com.google.gson.JsonParser
import java.math.BigDecimal
import java.text.NumberFormat
import javax.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmEnumerationType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.slf4j.LoggerFactory
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentVariable
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil

/** 
 * compute whether and how coercion should be generated
 */
class TclCoercionComputer {
	
	private val logger = LoggerFactory.getLogger(TclCoercionComputer)
	
	@Inject extension JvmTypeReferenceUtil typeReferenceUtil
	@Inject extension TclJsonUtil

	def initWith(Resource resource) {
		// in order to have access to the classpath (within the reference util) the resource (set) must be set
		typeReferenceUtil.initWith(resource)
	}

	def initWith(ResourceSet resourceSet) {
		// in order to have access to the classpath (within the reference util) the resource (set) must be set
		typeReferenceUtil.initWith(resourceSet)
	}

	/**
	 * is the coercion of sourceValue (in string representation) of sourceType into targetType possible?
	 */
	@VisibleForTesting 
	def boolean isValueCoercionPossible(JvmTypeReference targetType, JvmTypeReference sourceType, String sourceValue) {
		try {
			switch (targetType) {
				case targetType.isString:
					return true
				case targetType.isLong: {
					Long.valueOf(sourceValue) // exception caught by enclosing try
					return true
				}
				case targetType.isBoolean:
					return Boolean.TRUE.toString.equals(sourceValue) || Boolean.FALSE.toString.equals(sourceValue)
				case targetType.isInt: {
					Integer.valueOf(sourceValue) // exception caught by enclosing try
					return true
				}
				case targetType.isJsonType: {
					new JsonParser().parse(sourceValue) // exception caught by enclosing try
					return true
				}
				case targetType.isNumber: {
					NumberFormat.instance.parse(sourceValue) // exception caught by enclosing try
					return true
				}
				case targetType.isBigDecimal: {
					new BigDecimal(sourceValue) // exception caught by enclosing try
					return true
				}
				case targetType.isEnum: {
					val enumType = (targetType.type as JvmEnumerationType)
					return enumType.literals.exists[simpleName == sourceValue]
				}
			}
		} catch (Exception e) {
			logger.trace('''Exception while coercing value = '«sourceValue»' to enum type = '«targetType.qualifiedName»'.''', e)
			return false
		}
		return false
	}
	
	/** 
	 * is coercion possible as far as information is available?
	 * 
	 * StepContentVariables, which pass constants to the fixture can be checked for their value,
	 * whereas all other StepContents can only be checked for basic type coercion capabilities.
	 */
	def boolean isCoercionPossible(JvmTypeReference targetType, JvmTypeReference sourceType, StepContent content) {
		val typesAreCoercible = isTypeCoercionPossible(targetType, sourceType)
		if (typesAreCoercible && content instanceof StepContentVariable) {
			val value = (content as StepContentVariable).value
			return isValueCoercionPossible(targetType, sourceType, value)
		} else {
			return typesAreCoercible
		}
	}
	
	/**
	 * is the coercion of sourceType into targetType basically possible (only if the  (yet unknown) value can be interpreted within the target type)?
	 */
	def boolean isTypeCoercionPossible(JvmTypeReference targetType, JvmTypeReference sourceType) {
		switch targetType {
			case targetType.isString : return sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber || sourceType.isBoolean || sourceType.isJsonType || sourceType.isString || sourceType.isEnum || (sourceType.isObject && !sourceType.isMaskingString)
			case targetType.isLong: return sourceType.isString || sourceType.isJsonType || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber
			case targetType.isBoolean: return sourceType.isString || sourceType.isJsonType || sourceType.isBoolean
			case targetType.isInt: return sourceType.isString || sourceType.isJsonType || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber
			case targetType.isJsonType: return sourceType.isString || sourceType.isBoolean || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber || sourceType.isJsonType || sourceType.isEnum
			case targetType.isNumber: return sourceType.isString || sourceType.isJsonType || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber
			case targetType.isBigDecimal: return sourceType.isString || sourceType.isJsonType || sourceType.isLong || sourceType.isInt || sourceType.isBigDecimal || sourceType.isANumber
			case targetType.isEnum: return sourceType.isString || sourceType.isJsonType || sourceType.isEnum
			default: // ignore, simply return that coercion is not possible
				return false
		}
	}
	
	/**
	 * generate guard code that will check whether dynamic values can actually be coerced intot he target type 
	 * (for which static decisions cannot be made)
	 */
	def String generateCoercionGuard(JvmTypeReference targetType, JvmTypeReference sourceType, String sourceValue, String quotedErrorMessage) {
		val coercionErrorMessage = '''Coercion not possible from sourceType = '«sourceType?.qualifiedName»' to targetType= '«targetType?.qualifiedName»'.'''
		if (isTypeCoercionPossible(targetType, sourceType)) {
			switch targetType {
				case targetType.isInt:
					if (sourceType.isInt) {
						return ''
					} else if (sourceType.isLong) {
						return '''try { java.lang.Math.toIntExact(«sourceValue»); } catch (ArithmeticException ae) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else if (sourceType.isNumber) {
						return ''
					} else if (sourceType.isJsonType) {
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
					} else if (sourceType.isJsonType) {
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
					} else if (sourceType.isJsonType) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isBoolean());'''
					} else if (sourceType.isString) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», Boolean.TRUE.toString().equals(«sourceValue») || Boolean.FALSE.toString().equals(«sourceValue»));'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isString:
					if (sourceType.isJsonType) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isString());'''
					} else { // long, bool etc. need no guard, since they can all be converted to string
						return ''
					}
				case targetType.isJsonType: return '' // no guard for json, since that is parsed (and checked) by json library
				case targetType.isNumber:
					if (sourceType.isJsonType) {
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
					} else if (sourceType.isJsonType) {
						return '''org.junit.Assert.assertTrue(«quotedErrorMessage», «sourceValue».getAsJsonPrimitive().isNumber());'''
					} else if (sourceType.isString) {
						return '''try { new java.math.BigDecimal(«sourceValue»); } catch (NumberFormatException nfe) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isEnum:
					if (sourceType.isString) {
						return '''try { «targetType.qualifiedName».valueOf(«sourceValue»); } catch (IllegalArgumentException ia) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else if (sourceType.isJsonType) {
						return '''try { «targetType.qualifiedName».valueOf(«sourceValue»«generateJsonElementAccess(stringJvmTypeReference)»); } catch (IllegalArgumentException ia) { org.junit.Assert.fail(«quotedErrorMessage»); }'''
					} else if (sourceType.isEnum && isAssignableFrom(targetType, sourceType)) {
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
	
	/** generate coercion code for dynamically retrieved values for which static decisions cannot be made */
	def String generateCoercion(JvmTypeReference targetType, JvmTypeReference sourceType, String sourceValueAccess) {
		val coercionErrorMessage = '''Coercion not possible from sourceType = '«sourceType?.qualifiedName»' to targetType= '«targetType?.qualifiedName»'.'''
		if (isTypeCoercionPossible(targetType, sourceType)) {
			switch (targetType) {
				case targetType.isNumber:
					if (sourceType.isJsonType) {
						return '''«sourceValueAccess»«generateJsonElementAccess(targetType)»'''
					} else if (sourceType.isInt || sourceType.isLong || sourceType.isBigDecimal || sourceType.isNumber || sourceType.isANumber) {
						return sourceValueAccess
					} else if (sourceType.isString) {
						return '''java.text.NumberFormat.getInstance().parse(«sourceValueAccess»)'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isJsonType:
					if (sourceType.isString) {
						return jsonParseInstruction('''"\""+«sourceValueAccess»+"\""''')
					} else if (sourceType.isInt) {
						return jsonParseInstruction('''Integer.toString(«sourceValueAccess»)''')
					} else if (sourceType.isLong) {
						return jsonParseInstruction('''Long.toString(«sourceValueAccess»)''')
					} else if (sourceType.isBoolean) {
						return jsonParseInstruction('''Boolean.toString(«sourceValueAccess»)''')
					} else if (sourceType.isJsonType) {
						return sourceValueAccess
					} else if (sourceType.isNumber) {
						return jsonParseInstruction('''«sourceValueAccess»''')
					} else if (sourceType.isEnum) {
						return jsonParseInstruction('''"\""+«sourceValueAccess».toString()+"\""''')
					} else {
						return jsonParseInstruction('''«sourceValueAccess».toString()''')
					}
				case targetType.isInt:
					if (sourceType.isString) {
						return '''Integer.parseInt(«sourceValueAccess»)'''
					} else if (sourceType.isLong) {
						return '''java.lang.Math.toIntExact(«sourceValueAccess»)'''
					} else if (sourceType.isNumber) {
						return sourceValueAccess
					} else if (sourceType.isBigDecimal) {
						return '''«sourceValueAccess».intValueExact()'''
					} else if (sourceType.isInt) {
						return sourceValueAccess
					} else if (sourceType.isJsonType) {
						return '''«sourceValueAccess»«generateJsonElementAccess(targetType)»'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isLong:
					if (sourceType.isString) {
						return '''Long.parseLong(«sourceValueAccess»)'''
					} else if (sourceType.isNumber) {
						return sourceValueAccess
					} else if (sourceType.isLong || sourceType.isInt) {
						return sourceValueAccess
					} else if (sourceType.isBigDecimal) {
						return '''«sourceValueAccess».longValueExact()'''
					} else if (sourceType.isJsonType) {
						return '''«sourceValueAccess»«generateJsonElementAccess(targetType)»'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isBoolean:
					if (sourceType.isString) {
						return '''Boolean.valueOf(«sourceValueAccess»)'''
					} else if (sourceType.isBoolean) {
						return sourceValueAccess
					} else if (sourceType.isJsonType) {
						return '''«sourceValueAccess»«generateJsonElementAccess(targetType)»'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isString:
					if (sourceType.isString) {
						return sourceValueAccess
					} else if (sourceType.isInt) {
						return '''Integer.toString(«sourceValueAccess»)'''
					} else if (sourceType.isLong) {
						return '''Long.toString(«sourceValueAccess»)'''
					} else if (sourceType.isBoolean) {
						return '''Boolean.toString(«sourceValueAccess»)'''
					} else if (sourceType.isBigDecimal) {
						return '''«sourceValueAccess».toString()'''
					} else if (sourceType.isNumber) {
						return '''String.valueOf(«sourceValueAccess»)'''
					} else if (sourceType.isJsonType) {
						return '''«sourceValueAccess»«generateJsonElementAccess(targetType)»'''
					} else if (sourceType.isEnum) {
						return '''«sourceValueAccess».toString()'''
					} else if (sourceType.isObject) {
						return '''«sourceValueAccess».toString()'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isBigDecimal:
					if (sourceType.isBigDecimal) {
						return sourceValueAccess
					} else if (sourceType.isJsonType) {
						return '''«sourceValueAccess»«generateJsonElementAccess(targetType)»'''
					} else if (sourceType.isInt || sourceType.isLong || sourceType.isString || sourceType.isNumber) {
						return '''new java.math.BigDecimal(«sourceValueAccess»)'''
					} else {
						throw new RuntimeException(coercionErrorMessage)
					}
				case targetType.isEnum:
					if (sourceType.isString) {
						return '''«targetType.qualifiedName».valueOf(«sourceValueAccess»)'''
					} else if (sourceType.isJsonType) {
						return '''«targetType.qualifiedName».valueOf(«sourceValueAccess»«generateJsonElementAccess(stringJvmTypeReference)»)'''
					} else if ((sourceType.isEnum) && isAssignableFrom(targetType, sourceType)) {
						return '''«sourceValueAccess»'''
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
		return isTypeCoercionPossible(numberJvmTypeReference, typeReferenceA) &&
			isTypeCoercionPossible(numberJvmTypeReference, typeReferenceB)
	}
	
	def coercedCommonOrderableType(JvmTypeReference typeReferenceA, JvmTypeReference typeReferenceB) {
		if (coercableToCommonOrderable(typeReferenceA, typeReferenceB)) {
			// return numberJvmTypeReference
			if (typeReferenceA.isBigDecimal || typeReferenceB.isBigDecimal || typeReferenceA.isJsonType || typeReferenceB.isJsonType) {
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
		if (isTypeCoercionPossible(typeReferenceA, typeReferenceB)) {
			return typeReferenceA
		}
		if (isTypeCoercionPossible(typeReferenceB, typeReferenceA)) {
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
			(isTypeCoercionPossible(typeReferenceA, typeReferenceB)) ||
			(isTypeCoercionPossible(typeReferenceB, typeReferenceA)))
	}
	
}
