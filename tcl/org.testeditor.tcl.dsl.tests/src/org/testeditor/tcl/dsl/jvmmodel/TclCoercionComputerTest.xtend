package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmTypeReference
import org.junit.Before
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTclTest

class TclCoercionComputerTest extends AbstractTclTest {
	
	@Inject TclCoercionComputer coercionComputer // class under test
	@Inject extension TclJvmTypeReferenceUtil typeReferenceUtil

	// cannot use junit parameters since they have to be static which collides with injection
	def coercionIllegalData() {
		return #[
			// targetType                        sourceType
			// ---------------------------------------------------------------------
			// boolean <-> long is not allowed
			#[ longObjectJvmTypeReference,       booleanObjectJvmTypeReference],
			#[ longObjectJvmTypeReference,       booleanPrimitiveJvmTypeReference],
			#[ longPrimitiveJvmTypeReference,    booleanObjectJvmTypeReference],
			#[ longPrimitiveJvmTypeReference,    booleanPrimitiveJvmTypeReference],
			#[ booleanObjectJvmTypeReference,    longObjectJvmTypeReference],
			#[ booleanObjectJvmTypeReference,    longPrimitiveJvmTypeReference],
			#[ booleanPrimitiveJvmTypeReference, longObjectJvmTypeReference],
			#[ booleanPrimitiveJvmTypeReference, longPrimitiveJvmTypeReference]
		]
	}
	
	def coercionData() {
		return #[
			// targetType                        sourceType                        coercion ('data' is chosen arbitrarily, see tests)                  coercion guard
			// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
			// coercion to same type (always true)
			#[ booleanObjectJvmTypeReference,    booleanObjectJvmTypeReference,    'data',                                                             ''],
			#[ booleanPrimitiveJvmTypeReference, booleanPrimitiveJvmTypeReference, 'data',                                                             ''],
			#[ longObjectJvmTypeReference,       longObjectJvmTypeReference,       'data',                                                             ''],
			#[ longPrimitiveJvmTypeReference,    longPrimitiveJvmTypeReference,    'data',                                                             ''],
			#[ stringJvmTypeReference,           stringJvmTypeReference,           'data',                                                             ''],
			#[ jsonObjectJvmTypeReference,       jsonObjectJvmTypeReference,       'data',                                                             ''],
			
			// coercion from json (always true, access is done through expected type)
			#[ booleanObjectJvmTypeReference,    jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsBoolean()',                         'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isBoolean());'],
			#[ booleanPrimitiveJvmTypeReference, jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsBoolean()',                         'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isBoolean());'],
			#[ longObjectJvmTypeReference,       jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsLong()',                            'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ longPrimitiveJvmTypeReference,    jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsLong()',                            'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ stringJvmTypeReference,           jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsString()',                          'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isString());'],
			
			// coercion to string (always true)
			#[ stringJvmTypeReference,           booleanObjectJvmTypeReference,    'Boolean.toString(data)',                                           ''],
			#[ stringJvmTypeReference,           booleanPrimitiveJvmTypeReference, 'Boolean.toString(data)',                                           ''],
			#[ stringJvmTypeReference,           longObjectJvmTypeReference,       'Long.toString(data)',                                              ''],
			#[ stringJvmTypeReference,           longPrimitiveJvmTypeReference,    'Long.toString(data)',                                              ''],
			
			// coercion to json (needs parsing and thus a guard
			#[ jsonObjectJvmTypeReference,       booleanObjectJvmTypeReference,    'new com.google.gson.JsonParser().parse(Boolean.toString(data))',   ''],
			#[ jsonObjectJvmTypeReference,       booleanPrimitiveJvmTypeReference, 'new com.google.gson.JsonParser().parse(Boolean.toString(data))',   ''],
			#[ jsonObjectJvmTypeReference,       longObjectJvmTypeReference,       'new com.google.gson.JsonParser().parse(Long.toString(data))',      ''],
			#[ jsonObjectJvmTypeReference,       longPrimitiveJvmTypeReference,    'new com.google.gson.JsonParser().parse(Long.toString(data))',      ''],
			#[ jsonObjectJvmTypeReference,       stringJvmTypeReference,           'new com.google.gson.JsonParser().parse("\\""+data+"\\"")',         ''],
			
			// coercion from string (always true, needs parsing though)
			#[ booleanObjectJvmTypeReference,    stringJvmTypeReference,           'Boolean.valueOf(data)',                                            'org.junit.Assert.assertTrue("msg", Boolean.TRUE.toString().equals(data) || Boolean.FALSE.toString().equals(data));'],
			#[ booleanPrimitiveJvmTypeReference, stringJvmTypeReference,           'Boolean.valueOf(data)',                                            'org.junit.Assert.assertTrue("msg", Boolean.TRUE.toString().equals(data) || Boolean.FALSE.toString().equals(data));'],
			#[ longObjectJvmTypeReference,       stringJvmTypeReference,           'Long.parseLong(data)',                                             'try { Long.parseLong(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			#[ longPrimitiveJvmTypeReference,    stringJvmTypeReference,           'Long.parseLong(data)',                                             'try { Long.parseLong(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }']
		]
	}

	@Before
	def void initCoercionComputer() {
		coercionComputer.initWith(null as ResourceSet) // null is allowable for tests
		typeReferenceUtil.initWith(null as ResourceSet)
	}
	
	@Test
	def testIllegalCoercionImpossible() {
		coercionIllegalData.forEach[
			// given
			val target = get(0) as JvmTypeReference
			val source = get(1) as JvmTypeReference
			
			// when
			val result = coercionComputer.isCoercionPossible(target, source)

			// then
			assertFalse(result, '''Coercible should be impossible for targetType = '«target?.qualifiedName»' and sourceType = '«source?.qualifiedName»'. ''')
		]
	}
	
	@Test
	def testIllegalCoercion() {
		coercionIllegalData.forEach[
			// given
			val target = get(0) as JvmTypeReference
			val source = get(1) as JvmTypeReference
			
			// when
			try {
				coercionComputer.generateCoercion(target, source, 'data')
				fail('''Coercion generation must throw an exception for impossible coercion of targetType = '«target?.qualifiedName»' and sourceType = '«source?.qualifiedName»'. ''')
			}catch(Exception e){
				// ignore
			}
			
			// then ok
		]
	}
	
	@Test
	def testLegalCoercionIsPossible() {
		coercionData.forEach[
			// given
			val target = get(0) as JvmTypeReference
			val source = get(1) as JvmTypeReference

			// when
			val result = coercionComputer.isCoercionPossible(target, source)
			
			// then
			assertTrue(result, '''Coercion must be possible for targetType = '«target?.qualifiedName»' and sourceType = '«source?.qualifiedName»'. ''')
		]
	}

	@Test
	def testCoercion() {
		coercionData.forEach[
			// given
			val target = get(0) as JvmTypeReference
			val source = get(1) as JvmTypeReference
			val coercion = get(2) as String
			
			// when
			val result = coercionComputer.generateCoercion(target, source, 'data')
			
			// then
			assertEquals(result, coercion, '''Coercion should return = '«coercion»' for targetType = '«target?.qualifiedName»' and sourceType = '«source?.qualifiedName»'. ''')
		]
	}
	
	@Test
	def testCoercionGuard() {
		coercionData.forEach[
			// given
			val target = get(0) as JvmTypeReference
			val source = get(1) as JvmTypeReference
			val expectedGuard = get(3) as String
			
			// when
			val guard = coercionComputer.generateCoercionGuard(target, source, 'data', '"msg"')
			
			// then
			assertEquals(guard, expectedGuard, '''Generated guard failed for coercion of targetType = '«target?.qualifiedName»' and sourceType = '«source?.qualifiedName»'. ''')
		]
	}
}