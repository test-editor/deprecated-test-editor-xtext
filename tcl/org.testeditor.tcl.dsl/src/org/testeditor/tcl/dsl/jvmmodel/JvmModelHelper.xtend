package org.testeditor.tcl.dsl.jvmmodel

import java.util.List
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmVisibility

class JvmModelHelper {

	/**
	 * Collects all fields declared in super types that are accessible from the
	 * passed type.
	 * 
	 * @return a list of all accessible fields declared in super classes
	 */
	def List<JvmField> getAllAccessibleSuperTypeFields(JvmDeclaredType type) {
		val result = newLinkedList
		type.superClasses.forEach[collectAccessibleFields(type, result)]
		return result
	}

	private def Iterable<JvmDeclaredType> getSuperClasses(JvmDeclaredType type) {
		return type.superTypes.map[it.type].filter(JvmDeclaredType)
	}

	private def void collectAccessibleFields(JvmDeclaredType type, JvmDeclaredType sourceType, List<JvmField> result) {
		result += type.members.filter(JvmField).filter[isAccessibleBy(sourceType)]
		type.superClasses.forEach[collectAccessibleFields(type, result)]
	}

	private def boolean isAccessibleBy(JvmField field, JvmDeclaredType sourceType) {
		if (field.visibility == JvmVisibility.PRIVATE) {
			return false
		}
		if (field.visibility == JvmVisibility.DEFAULT) {
			return field.declaringType.packageName == sourceType.packageName
		}
		return true
	}

}
