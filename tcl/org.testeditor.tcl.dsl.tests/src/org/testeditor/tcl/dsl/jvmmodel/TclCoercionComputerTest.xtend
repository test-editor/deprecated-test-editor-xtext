package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmTypeReference
import org.junit.Before
import org.junit.Test
import org.eclipse.xtext.util.Pair
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.eclipse.xtext.util.Tuples

class TclCoercionComputerTest extends AbstractTclTest {
	
	@Inject TclCoercionComputer coercionComputer // class under test
	@Inject extension TclJvmTypeReferenceUtil typeReferenceUtil
	
	def allKnownTypes() {
		return #[
			booleanPrimitiveJvmTypeReference,
			booleanObjectJvmTypeReference,
			intPrimitiveJvmTypeReference,
			intObjectJvmTypeReference,
			longPrimitiveJvmTypeReference,
			longObjectJvmTypeReference,
			bigDecimalJvmTypeReference,
			numberJvmTypeReference,
			stringJvmTypeReference,
			jsonObjectJvmTypeReference
		]
	}

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
			#[ booleanPrimitiveJvmTypeReference, longPrimitiveJvmTypeReference],
			#[ intObjectJvmTypeReference,        booleanObjectJvmTypeReference],
			#[ intObjectJvmTypeReference,        booleanPrimitiveJvmTypeReference],
			#[ intPrimitiveJvmTypeReference,     booleanObjectJvmTypeReference],
			#[ intPrimitiveJvmTypeReference,     booleanPrimitiveJvmTypeReference],
			#[ booleanObjectJvmTypeReference,    intObjectJvmTypeReference],
			#[ booleanObjectJvmTypeReference,    intPrimitiveJvmTypeReference],
			#[ booleanPrimitiveJvmTypeReference, intObjectJvmTypeReference],
			#[ booleanPrimitiveJvmTypeReference, intPrimitiveJvmTypeReference]
		]
	}
	
	def coercionData() {
		return #[
			// targetType                        sourceType                        coercion ('data' is chosen arbitrarily, see tests)                  coercion guard
			// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
			// coercion to same type (always true)
			#[ booleanObjectJvmTypeReference,    booleanObjectJvmTypeReference,    'data',                                                             ''],
			#[ booleanPrimitiveJvmTypeReference, booleanPrimitiveJvmTypeReference, 'data',                                                             ''],
			#[ intObjectJvmTypeReference,        intObjectJvmTypeReference,        'data',                                                             ''],
			#[ intPrimitiveJvmTypeReference,     intPrimitiveJvmTypeReference,     'data',                                                             ''],
			#[ longObjectJvmTypeReference,       longObjectJvmTypeReference,       'data',                                                             ''],
			#[ longPrimitiveJvmTypeReference,    longPrimitiveJvmTypeReference,    'data',                                                             ''],
			#[ stringJvmTypeReference,           stringJvmTypeReference,           'data',                                                             ''],
			#[ jsonObjectJvmTypeReference,       jsonObjectJvmTypeReference,       'data',                                                             ''],
			
			// coercion from json (always true, access is done through expected type)
			#[ booleanObjectJvmTypeReference,    jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsBoolean()',                         'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isBoolean());'],
			#[ booleanPrimitiveJvmTypeReference, jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsBoolean()',                         'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isBoolean());'],
			#[ intObjectJvmTypeReference,        jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsInt()',                             'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ intPrimitiveJvmTypeReference,     jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsInt()',                             'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ longObjectJvmTypeReference,       jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsLong()',                            'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ longPrimitiveJvmTypeReference,    jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsLong()',                            'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ stringJvmTypeReference,           jsonObjectJvmTypeReference,       'data.getAsJsonPrimitive().getAsString()',                          'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isString());'],
			
			// coercion to string (always true)
			#[ stringJvmTypeReference,           booleanObjectJvmTypeReference,    'Boolean.toString(data)',                                           ''],
			#[ stringJvmTypeReference,           booleanPrimitiveJvmTypeReference, 'Boolean.toString(data)',                                           ''],
			#[ stringJvmTypeReference,           longObjectJvmTypeReference,       'Long.toString(data)',                                              ''],
			#[ stringJvmTypeReference,           longPrimitiveJvmTypeReference,    'Long.toString(data)',                                              ''],
			#[ stringJvmTypeReference,           intObjectJvmTypeReference,        'Integer.toString(data)',                                           ''],
			#[ stringJvmTypeReference,           intPrimitiveJvmTypeReference,     'Integer.toString(data)',                                           ''],
			
			// coercion to json (needs parsing and thus a guard
			#[ jsonObjectJvmTypeReference,       booleanObjectJvmTypeReference,    'new com.google.gson.JsonParser().parse(Boolean.toString(data))',   ''],
			#[ jsonObjectJvmTypeReference,       booleanPrimitiveJvmTypeReference, 'new com.google.gson.JsonParser().parse(Boolean.toString(data))',   ''],
			#[ jsonObjectJvmTypeReference,       intObjectJvmTypeReference,        'new com.google.gson.JsonParser().parse(Integer.toString(data))',   ''],
			#[ jsonObjectJvmTypeReference,       intPrimitiveJvmTypeReference,     'new com.google.gson.JsonParser().parse(Integer.toString(data))',   ''],
			#[ jsonObjectJvmTypeReference,       longObjectJvmTypeReference,       'new com.google.gson.JsonParser().parse(Long.toString(data))',      ''],
			#[ jsonObjectJvmTypeReference,       longPrimitiveJvmTypeReference,    'new com.google.gson.JsonParser().parse(Long.toString(data))',      ''],
			#[ jsonObjectJvmTypeReference,       stringJvmTypeReference,           'new com.google.gson.JsonParser().parse("\\""+data+"\\"")',         ''],
			
			// coercion from string (always true, needs parsing though)
			#[ booleanObjectJvmTypeReference,    stringJvmTypeReference,           'Boolean.valueOf(data)',                                            'org.junit.Assert.assertTrue("msg", Boolean.TRUE.toString().equals(data) || Boolean.FALSE.toString().equals(data));'],
			#[ booleanPrimitiveJvmTypeReference, stringJvmTypeReference,           'Boolean.valueOf(data)',                                            'org.junit.Assert.assertTrue("msg", Boolean.TRUE.toString().equals(data) || Boolean.FALSE.toString().equals(data));'],
			#[ intObjectJvmTypeReference,        stringJvmTypeReference,           'Integer.parseInt(data)',                                           'try { Integer.parseInt(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			#[ intPrimitiveJvmTypeReference,     stringJvmTypeReference,           'Integer.parseInt(data)',                                           'try { Integer.parseInt(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			#[ longObjectJvmTypeReference,       stringJvmTypeReference,           'Long.parseLong(data)',                                             'try { Long.parseLong(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			#[ longPrimitiveJvmTypeReference,    stringJvmTypeReference,           'Long.parseLong(data)',                                             'try { Long.parseLong(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			
			// coercion long <-> int
			#[ intObjectJvmTypeReference,        longObjectJvmTypeReference,       'java.lang.Math.toIntExact(data)',                                  'try { java.lang.Math.toIntExact(data); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ intPrimitiveJvmTypeReference,     longObjectJvmTypeReference,       'java.lang.Math.toIntExact(data)',                                  'try { java.lang.Math.toIntExact(data); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ intObjectJvmTypeReference,        longPrimitiveJvmTypeReference,    'java.lang.Math.toIntExact(data)',                                  'try { java.lang.Math.toIntExact(data); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ intPrimitiveJvmTypeReference,     longPrimitiveJvmTypeReference,    'java.lang.Math.toIntExact(data)',                                  'try { java.lang.Math.toIntExact(data); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			
			#[ longObjectJvmTypeReference,       intObjectJvmTypeReference,        'data',                                                             ''],
			#[ longPrimitiveJvmTypeReference,    intObjectJvmTypeReference,        'data',                                                             ''],
			#[ longObjectJvmTypeReference,       intPrimitiveJvmTypeReference,     'data',                                                             ''],
			#[ longPrimitiveJvmTypeReference,    intPrimitiveJvmTypeReference,     'data',                                                             '']
		]
	}

	@Before
	def void initCoercionComputer() {
		coercionComputer.initWith(null as ResourceSet) // null is allowable for tests but has some restrictions (assignable does not work as it should)
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
	
	@Test
	def testThatAllCombinationsAreCoercibleOrNonCoercible() {
		// ------------- given
		allPossibleTypePairs.forEach[pair|
			val typeA = pair.first
			val typeB = pair.second
			
			// ------------- when 
			val coercible = coercionComputer.isCoercionPossible(typeA, typeB)
			if (coercible) {
				try {
					// ------------- then 
					val coercion = coercionComputer.generateCoercion(typeA, typeB, 'some')
					val guard = coercionComputer.generateCoercionGuard(typeA, typeB, 'some', 'other')
					coercion.assertNotNull
					guard.assertNotNull
				} catch (Exception e) {
					fail('''Exception during coercion of a combination which was regarded possible (from='«typeB.qualifiedName»', to='«typeA.qualifiedName»').''')
				}
			} else {
				try {
					// ------------- else 
					coercionComputer.generateCoercion(typeA, typeB, 'some')
					fail('''Should run into an exception since coercion from = '«typeB.qualifiedName»' to '«typeA.qualifiedName»' is deemed impossible.'''.toString)
				} catch(Exception e) {
					// ignore, since this is ok
				}
				try {
					// ------------- else 
					coercionComputer.generateCoercionGuard(typeA, typeB, 'some', 'other')
					fail('''Should run into an exception since coercion guard from = '«typeB.qualifiedName»' to '«typeA.qualifiedName»' is deemed impossible.'''.toString)
				} catch(Exception e) {
					// ignore, since this is ok
				}
			}
		]
	}
	
	def Iterable<Pair<JvmTypeReference,JvmTypeReference>> getAllPossibleTypePairs() {
		allKnownTypes.map[typeA |
			allKnownTypes.map[ typeB |
				Tuples.create(typeA,typeB)
			]
		].flatten
	} 
	
}