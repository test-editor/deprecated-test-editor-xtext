package org.testeditor.aml.model.test

import com.google.inject.Provider
import java.util.List
import java.util.Map
import com.google.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.dsl.tests.AbstractAmlTest

/** placed here since one test class does not justify a new plugin (may change) */
class ModelUtilTest extends AbstractAmlTest {
	@Inject ModelUtil classUnderTest

	@Inject JvmTypeReferenceBuilder.Factory jvmTypeReferenceBuilderFactory
	@Inject Provider<ResourceSet> resourceSetProvider

	var JvmTypeReferenceBuilder jvmTypeReferenceBuilder

	@Before
	def void setUp() {
		jvmTypeReferenceBuilder = jvmTypeReferenceBuilderFactory.create(resourceSetProvider.get)
	}

	@Test
	def void testGenericMapAssignable() {
		Map.assertAssignable("java.util.Map", true, "String", "Object")
	}

	@Test
	def void testGenericListAssignable() {
		List.assertAssignable("java.util.List", true, "String")
	}

	@Test
	def void testboolAssignable() {
		boolean.assertAssignable("boolean", true)
	}

	@Test
	def void testintNotAssignableToLong() {
		long.assertAssignable("int", false)
	}

	@Test
	def void testintAssignable() {
		int.assertAssignable("int", true)
	}

	@Test
	def void testBooleanAssignable() {
		Boolean.assertAssignable("java.lang.Boolean", true)
	}

	@Test
	def void testBooleanAssignableToObject() {
		Object.assertAssignable("java.lang.Boolean", true)
	}

	@Test
	def void testObjectNotAssignableToBool() {
		Boolean.assertAssignable("java.lang.Object", false)
	}

	private def void assertAssignable(Class<?> clazz, String classString, boolean positive, String... genericTypeParameter) {
		// given 
		val typeRef = jvmTypeReferenceBuilder.typeRef(classString, genericTypeParameter.map[jvmTypeReferenceBuilder.typeRef(it)])

		// when
		val result = classUnderTest.isAssignableWithoutConversion(clazz, typeRef)

		// then
		Assert.assertTrue(positive == result)
	}
}
