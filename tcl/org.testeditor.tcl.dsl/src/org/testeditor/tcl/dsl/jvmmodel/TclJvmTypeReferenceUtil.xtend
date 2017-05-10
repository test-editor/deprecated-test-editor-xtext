package org.testeditor.tcl.dsl.jvmmodel

import com.google.gson.JsonArray
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.JsonPrimitive
import javax.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.JvmUnknownTypeReference
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices

// @Singleton
class TclJvmTypeReferenceUtil {

	@Inject TclJsonUtil tclJsonUtil
	@Inject CommonTypeComputationServices services
	var StandardTypeReferenceOwner typeReferenceOwner
	var JvmTypeReferenceBuilder typeReferenceBuilder
	
	@Inject ResourceSet resourceSet // useful for testing with models without actual resource set
	@Inject JvmTypeReferenceBuilder.Factory typeReferenceBuilderFactory
	
	def void initWith(ResourceSet resourceSet) {
		// cache a valid typeReferenceBuilder ?
		if (resourceSet === null) {
			typeReferenceBuilder = typeReferenceBuilderFactory.create(this.resourceSet) // useful for testing with models without resources
		} else {
			typeReferenceBuilder = typeReferenceBuilderFactory.create(resourceSet)
		}
	}
	
	def void initWith(Resource resource) {
		if(resource === null) {
			initWith(resourceSet)
		} else  {
			initWith(resource.resourceSet)
		}
	}
	
	private def ensureTypeReferenceBuilderInitialized() {
		if (typeReferenceBuilder === null) {
			throw new RuntimeException("Please call initWith beforehand!")
		}
	}
	
	def JvmTypeReference buildFrom(Class<?> clazz, JvmTypeReference ... typeArgs) { // allow varargs (which is not supported by create methods, or so it seems)
		return _buildFrom(clazz, typeArgs)
	}

	private def JvmTypeReference create typeReferenceBuilder.typeRef(clazz, typeArgs) _buildFrom(Class<?> clazz, JvmTypeReference[] typeArgs) {
		if (it instanceof JvmUnknownTypeReference) {
			throw new RuntimeException('''Cannot create reference for type '«clazz.name»', initWith was probably called with wrong resourceset.''')
		} 
	}

	def boolean isJsonArray(JvmTypeReference typeReference) {
		jsonArrayJvmTypeReference.equals(typeReference) // let's see if this works
	}

	def boolean isJsonObject(JvmTypeReference typeReference) {
		jsonObjectJvmTypeReference.equals(typeReference)
	}

	def boolean isJsonPrimitive(JvmTypeReference typeReference) {
		jsonPrimitiveJvmTypeReference.equals(typeReference)
	}

	def boolean isJsonElement(JvmTypeReference typeReference) {
		jsonElementJvmTypeReference.equals(typeReference)
	}

	def boolean isString(JvmTypeReference typeReference) {
		return String.name.equals(typeReference?.qualifiedName)
	}

	def boolean isBoolean(JvmTypeReference typeReference) {
		val qname = typeReference?.qualifiedName
		return (Boolean.name == qname || boolean.name == qname)
	}

	def boolean isLong(JvmTypeReference typeReference) {
		val qname = typeReference.qualifiedName
		return (Long.name == qname || long.name == qname)
	}

	def boolean isJson(JvmTypeReference typeReference) {
		return tclJsonUtil.isJsonType(typeReference)
	}

	def boolean isOrderable(JvmTypeReference typeReference) {
		return isLong(typeReference) // currently the only one known to be orderable is long
	}

	def boolean isAssignableFrom(JvmTypeReference target, JvmTypeReference source,
		TypeConformanceComputationArgument argument) {
		if (typeReferenceOwner === null) {
			typeReferenceOwner = new StandardTypeReferenceOwner(services, null as ResourceSet)
		}
		if (target.qualifiedName == source.qualifiedName) { // workaround for a bug, where two types that boil down to java.lang.String were deemed NOT assignable
			return true
		}
		val lleft = typeReferenceOwner.toLightweightTypeReference(target)
		var lright = typeReferenceOwner.toLightweightTypeReference(source)
		val assignable = lleft.isAssignableFrom(lright, argument)
		return assignable
	}

	// use xbase to check for assignablility
	def boolean isAssignableFrom(JvmTypeReference target, JvmTypeReference source) {
		return isAssignableFrom(target, source, new TypeConformanceComputationArgument)
	}
	
	def isNonFractionalNumber(JvmTypeReference reference) {
		return isLong(reference) || isInt(reference)
	}
	
	def isInt(JvmTypeReference reference) {
		val qname = reference?.qualifiedName
		return int.name.equals(qname) || Integer.name.equals(qname)
	}
	
	def JvmTypeReference booleanPrimitiveJvmTypeReference() { ensureTypeReferenceBuilderInitialized return boolean.buildFrom }
	def JvmTypeReference booleanObjectJvmTypeReference() { ensureTypeReferenceBuilderInitialized return Boolean.buildFrom }
	def JvmTypeReference stringJvmTypeReference() { ensureTypeReferenceBuilderInitialized return String.buildFrom }
	def JvmTypeReference longPrimitiveJvmTypeReference() { ensureTypeReferenceBuilderInitialized return long.buildFrom } 
	def JvmTypeReference longObjectJvmTypeReference() { ensureTypeReferenceBuilderInitialized return Long.buildFrom }
	def JvmTypeReference jsonElementJvmTypeReference() { ensureTypeReferenceBuilderInitialized return JsonElement.buildFrom }
	def JvmTypeReference jsonPrimitiveJvmTypeReference() { ensureTypeReferenceBuilderInitialized return JsonPrimitive.buildFrom }
	def JvmTypeReference jsonObjectJvmTypeReference() { ensureTypeReferenceBuilderInitialized return JsonObject.buildFrom }
	def JvmTypeReference jsonArrayJvmTypeReference() { ensureTypeReferenceBuilderInitialized return JsonArray.buildFrom }

}
