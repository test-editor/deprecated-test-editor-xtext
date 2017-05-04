package org.testeditor.tcl.dsl.jvmmodel

import java.util.Map
import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices

/**
 * record several types with a fuzzy value to find matches in two such type descriptions
 */
class FuzzyTypeDescription {

	public static val DEFAULT_FUZZY_VALUE = 1000
	public static val ILLEGAL_FUZZY_VALUE = -1

	@Inject CommonTypeComputationServices services
	val Map<String, Pair<JvmTypeReference, Integer>> fuzzyTypes = newHashMap
	var StandardTypeReferenceOwner typeReferenceOwner
	
	protected new() { // please use injection
	}
	
	private def FuzzyTypeDescription newInstance() {
		return new FuzzyTypeDescription => [
			it.services = services
		]
	}

	def void addType(JvmTypeReference typeReference) {
		addType(typeReference, DEFAULT_FUZZY_VALUE)
	}

	def void addType(JvmTypeReference typeReference, int fuzzyValue) {
		fuzzyTypes.put(typeReference.qualifiedNonGenericName, new Pair(typeReference, fuzzyValue))
	}

	def Iterable<Pair<JvmTypeReference, Integer>> getAllTypes() {
		return fuzzyTypes.values
	}

	def Iterable<JvmTypeReference> getMostLikelyTypes() {
		val orderedTypeList = fuzzyTypes.values.sortWith[a, b|b.value - a.value]
		val max = orderedTypeList.head?.value ?: ILLEGAL_FUZZY_VALUE
		return orderedTypeList.filter[value == max].map[key]
	}

	def boolean matches(JvmTypeReference other) {
		fuzzyTypes.containsKey(other.qualifiedNonGenericName)
	}

	// use xbase to check for assignablility
	private def boolean isAssignableFrom(JvmTypeReference left, JvmTypeReference right) {
		if (typeReferenceOwner === null) {
			typeReferenceOwner = new StandardTypeReferenceOwner(services, null as ResourceSet)
		}
		val lleft = typeReferenceOwner.toLightweightTypeReference(left)
		var lright = typeReferenceOwner.toLightweightTypeReference(right)
		val assignable = lleft.isAssignableFrom(lright)
		return assignable
	}

	def Iterable<JvmTypeReference> findBestMatches(FuzzyTypeDescription typeDescription) {
		val resultingFuzzyTypeDescription = newInstance

		allTypes.forEach[resultingFuzzyTypeDescription.addFuzzyValueToType(key, value)]
		typeDescription.allTypes.forEach[other|
			val otherTypes = allTypes.filter[key.qualifiedName != other.key.qualifiedName]
			val assignableOtherTypes = otherTypes.filter[key.isAssignableFrom(other.key)]
			val addValue = assignableOtherTypes.map[value].reduce[p1, p2|p1+p2] ?: 0
			resultingFuzzyTypeDescription.addFuzzyValueToType(other.key, other.value+addValue)
		]

		return resultingFuzzyTypeDescription.mostLikelyTypes
	}

	private def void addFuzzyValueToType(JvmTypeReference typeReference, int additionalFuzzyValue) {
		val typeString = typeReference.qualifiedNonGenericName
		val fuzzyValue = if (fuzzyTypes.containsKey(typeString)) {
				fuzzyTypes.get(typeString).value
			} else {
				0
			}
		addType(typeReference, additionalFuzzyValue + fuzzyValue)
	}

	private def String getQualifiedNonGenericName(JvmTypeReference typeReference) {
		return typeReference?.qualifiedName//?.replaceAll('<.*', '')
	}

}
