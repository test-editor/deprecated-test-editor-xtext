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
package org.testeditor.dsl.common.util

import com.google.gson.JsonArray
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.JsonPrimitive
import java.math.BigDecimal
import javax.inject.Inject
import javax.inject.Singleton
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.common.types.JvmEnumerationType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices
import org.testeditor.fixture.core.MaskingString
import org.slf4j.LoggerFactory

/**
 * provide basic utility functions for JvmTypeReference(s)
 * 
 * since most functions make use of the type reference builder, please make sure
 * that 'initWith' is called with the current resouce or resouceSet to allow classpath resolution
 * of all relevant types.
 */
@Singleton
class JvmTypeReferenceUtil {
	
	private static val log = LoggerFactory.getLogger(JvmTypeReferenceUtil)

	@Accessors(PUBLIC_GETTER)
	val checkWithoutBoxing = new TypeConformanceComputationArgument(false, false, false, false, false, true)

	@Inject CommonTypeComputationServices services
	var StandardTypeReferenceOwner typeReferenceOwner
	var JvmTypeReferenceBuilder typeReferenceBuilder
	
	@Inject ResourceSet resourceSet // injection useful for testing with models without actual resource set
	@Inject JvmTypeReferenceBuilder.Factory typeReferenceBuilderFactory
	
	def void initWith(ResourceSet resourceSet) {
		// cache a valid typeReferenceBuilder ?
		if (resourceSet === null) {
			log.debug('''type reference builder initialized with injected resource set.''')
			typeReferenceBuilder = typeReferenceBuilderFactory.create(this.resourceSet) // useful for testing with models without resources
		} else if (resourceSet !== this.resourceSet || typeReferenceBuilder === null) {
			log.debug('''type reference build (re)initialized with different resource set.''')
			typeReferenceBuilder = typeReferenceBuilderFactory.create(resourceSet)
			this.resourceSet = resourceSet // overwrite injected one (which was injected for test purposes only)
			this.typeReferenceOwner = null // failure to do this will result in different instances of type references, failing in object identity comparisons within the rcp
		} else {
			log.trace('''reusing cached type reference builder.''')
		}
	}
	
	def void initWith(Resource resource) {
		if(resource === null) {
			initWith(this.resourceSet)
		} else  {
			initWith(resource?.resourceSet)
		}
	}
	
	private def ensureTypeReferenceBuilderInitialized() {
		if (typeReferenceBuilder === null) {
			throw new RuntimeException("Please call initWith beforehand!")
		}
	}
	
	def JvmTypeReference buildFrom(Class<?> clazz, JvmTypeReference ... typeArgs) {
		ensureTypeReferenceBuilderInitialized
		return typeReferenceBuilder.typeRef(clazz, typeArgs) // type reference build is already caching
	}

	def boolean isJsonArray(JvmTypeReference typeReference) {
		jsonArrayJvmTypeReference.qualifiedName == typeReference?.qualifiedName
	}

	def boolean isJsonObject(JvmTypeReference typeReference) {
		jsonObjectJvmTypeReference.qualifiedName == typeReference?.qualifiedName
	}

	def boolean isJsonPrimitive(JvmTypeReference typeReference) {
		jsonPrimitiveJvmTypeReference.qualifiedName == typeReference?.qualifiedName
	}

	def boolean isJsonElement(JvmTypeReference typeReference) {
		jsonElementJvmTypeReference.qualifiedName == typeReference?.qualifiedName
	}

	def boolean isString(JvmTypeReference typeReference) {
		return String.name == typeReference?.qualifiedName
	}

	def boolean isBoolean(JvmTypeReference typeReference) {
		val qname = typeReference?.qualifiedName
		return (Boolean.name == qname || boolean.name == qname)
	}

	def boolean isLong(JvmTypeReference typeReference) {
		val qname = typeReference.qualifiedName
		return (Long.name == qname || long.name == qname)
	}

	def boolean isOrderable(JvmTypeReference typeReference) {
		return typeReference.isANumber
	}
	
	// use xbase to check for assignablility
	def boolean isAssignableFrom(JvmTypeReference target, JvmTypeReference source,
		TypeConformanceComputationArgument argument) {
		if (target === null || source === null) {
			throw new RuntimeException("For testing assignment source and target types must not be null")
		}
		if (typeReferenceOwner === null) {
			typeReferenceOwner = new StandardTypeReferenceOwner(services, resourceSet)
		}
		val lleft = typeReferenceOwner.toLightweightTypeReference(target)
		var lright = typeReferenceOwner.toLightweightTypeReference(source)
		val assignable = lleft.isAssignableFrom(lright, argument)
		return assignable
	}

	def boolean isAssignableFrom(JvmTypeReference target, JvmTypeReference source) {
		return isAssignableFrom(target, source, new TypeConformanceComputationArgument)
	}
		
	def boolean isBigDecimal(JvmTypeReference reference) {
		val qname = reference?.qualifiedName
		return BigDecimal.name == qname
	}
	
	def boolean isInt(JvmTypeReference reference) {
		val qname = reference?.qualifiedName
		return int.name == qname || Integer.name == qname
	}
	
	def JvmTypeReference booleanPrimitiveJvmTypeReference() {
		return boolean.buildFrom
	}

	def JvmTypeReference booleanObjectJvmTypeReference() {
		return Boolean.buildFrom
	}

	def JvmTypeReference stringJvmTypeReference() {
		return String.buildFrom
	}

	def JvmTypeReference longPrimitiveJvmTypeReference() {
		return long.buildFrom
	}

	def JvmTypeReference longObjectJvmTypeReference() {
		return Long.buildFrom
	}

	def JvmTypeReference intPrimitiveJvmTypeReference() {
		return int.buildFrom
	}

	def JvmTypeReference intObjectJvmTypeReference() {
		return Integer.buildFrom
	}

	def JvmTypeReference jsonElementJvmTypeReference() {
		return JsonElement.buildFrom
	}

	def JvmTypeReference jsonPrimitiveJvmTypeReference() {
		return JsonPrimitive.buildFrom
	}

	def JvmTypeReference jsonObjectJvmTypeReference() {
		return JsonObject.buildFrom
	}

	def JvmTypeReference jsonArrayJvmTypeReference() {
		return JsonArray.buildFrom
	}
	
	def JvmTypeReference bigDecimalJvmTypeReference() {
		return BigDecimal.buildFrom
	}
	
	def JvmTypeReference enumJvmTypeReference() {
		return Enum.buildFrom
	}
	
	def JvmTypeReference maskingStringJvmTypeReference() {
		return MaskingString.buildFrom
	}
	
	def JvmTypeReference numberJvmTypeReference() {
		return Number.buildFrom
	}
	
	def JvmTypeReference objectJvmTypeReference() {
		return Object.buildFrom
	}
	
	def boolean isObject(JvmTypeReference reference) {
		return objectJvmTypeReference.isAssignableFrom(reference)
	}
	
	def boolean isMaskingString(JvmTypeReference reference) {
		return MaskingString.name.equals(reference?.qualifiedName)
	}

	def boolean isEnum(JvmTypeReference reference) {
		return enumJvmTypeReference.isAssignableFrom(reference)
	}

	def boolean isNumber(JvmTypeReference reference) {
		return Number.name.equals(reference?.qualifiedName)
	}
	
	def boolean isANumber(JvmTypeReference reference) {
		return numberJvmTypeReference.isAssignableFrom(reference) // including boxing
	}
	
	def Iterable<String> getEnumValues(JvmTypeReference reference) {
		val enumType = reference.type as JvmEnumerationType
		return enumType.literals.map[simpleName]
	}

}
