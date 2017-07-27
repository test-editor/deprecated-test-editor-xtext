package org.testeditor.tcl.dsl.jvmmodel

import java.time.DayOfWeek
import javax.inject.Inject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.util.Pair
import org.eclipse.xtext.util.Tuples
import org.junit.Before
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTclTest

class TclCoercionComputerTest extends AbstractTclTest {
	
	@Inject TclCoercionComputer coercionComputer // class under test
	@Inject extension TclJvmTypeReferenceUtil typeReferenceUtil
	
	private def Iterable<JvmTypeReference> booleanTypes() {
		return #[
			booleanPrimitiveJvmTypeReference, 
			booleanObjectJvmTypeReference
		]
	}
	
	private def Iterable<JvmTypeReference> numericTypes() {
		return #[
			intPrimitiveJvmTypeReference,
			intObjectJvmTypeReference,
			longPrimitiveJvmTypeReference,
			longObjectJvmTypeReference,
			bigDecimalJvmTypeReference,
			numberJvmTypeReference
		]
	}
	
	private def Iterable<JvmTypeReference> allKnownTypes() {
		return #[
			stringJvmTypeReference,
			jsonElementJvmTypeReference, // as a representative of all json object types
			enumJvmTypeReference
		] + booleanTypes + numericTypes
	}

	private def Iterable<Pair<JvmTypeReference, JvmTypeReference>> illegalCoercionTypePairs() {
		// illegal coercion is boolean <-> numeric type in any permutation 
		return getAllPairs(booleanTypes, numericTypes)
		       + getAllPairs(numericTypes, booleanTypes)
		       + getAllPairs(booleanTypes + numericTypes, #[enumJvmTypeReference])
		       + getAllPairs(#[enumJvmTypeReference], booleanTypes + numericTypes)
	}
	
	private def Iterable<Iterable<Object>> validValueFromStringCoercion() {
		return
		#[
			#[ intPrimitiveJvmTypeReference,           "123" ],
			#[ intObjectJvmTypeReference,              "123" ],
			#[ longPrimitiveJvmTypeReference,          "123" ],
			#[ longObjectJvmTypeReference,             "123" ],
			#[ booleanPrimitiveJvmTypeReference,       "true" ],
			#[ booleanObjectJvmTypeReference,          "false" ],
			#[ numberJvmTypeReference,                 "123" ],
			#[ stringJvmTypeReference,                 "some string" ],
			#[ jsonElementJvmTypeReference,            '''{ "key": 123 }'''.toString ],
			#[ bigDecimalJvmTypeReference,             "12323948573945867543985794328579438575845" ],
			#[ typeReferenceUtil.buildFrom(DayOfWeek), "MONDAY" ]
		]
	}
	
	private def Iterable<Iterable<Object>> invalidValueFromStringCoercion() {
		return
		#[
			#[ intPrimitiveJvmTypeReference,           "123e5" ],
			#[ intObjectJvmTypeReference,              "A00" ],
			#[ longPrimitiveJvmTypeReference,          "HEX" ],
			#[ longObjectJvmTypeReference,             "123e7" ],
			#[ booleanPrimitiveJvmTypeReference,       "truthy" ],
			#[ booleanObjectJvmTypeReference,          "falsy" ],
			#[ numberJvmTypeReference,                 "a123xy10" ],
			#[ jsonElementJvmTypeReference,            "no json" ],
			#[ bigDecimalJvmTypeReference,             "aha" ],
			#[ typeReferenceUtil.buildFrom(DayOfWeek), "MONDAX" ]
		]
	}
	
	private def Iterable<Iterable<Object>> legalCoercionData() {
		return #[
			// targetType                        sourceType                        coercion ('data' is chosen arbitrarily, see tests)                  coercion guard
			// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
			// coercion to same type (always true)
			#[ booleanObjectJvmTypeReference,    booleanObjectJvmTypeReference,    'data',                                                             ''],
			#[ booleanPrimitiveJvmTypeReference, booleanPrimitiveJvmTypeReference, 'data',                                                             ''],
			#[ booleanObjectJvmTypeReference,    booleanPrimitiveJvmTypeReference, 'data',                                                             ''],
			#[ booleanPrimitiveJvmTypeReference, booleanObjectJvmTypeReference,    'data',                                                             ''],
			
			#[ intObjectJvmTypeReference,        intObjectJvmTypeReference,        'data',                                                             ''],
			#[ intPrimitiveJvmTypeReference,     intPrimitiveJvmTypeReference,     'data',                                                             ''],
			#[ intObjectJvmTypeReference,        intPrimitiveJvmTypeReference,     'data',                                                             ''],
			#[ intPrimitiveJvmTypeReference,     intObjectJvmTypeReference,        'data',                                                             ''],
			
			#[ longObjectJvmTypeReference,       longObjectJvmTypeReference,       'data',                                                             ''],
			#[ longPrimitiveJvmTypeReference,    longPrimitiveJvmTypeReference,    'data',                                                             ''],
			#[ longObjectJvmTypeReference,       longPrimitiveJvmTypeReference,    'data',                                                             ''],
			#[ longPrimitiveJvmTypeReference,    longObjectJvmTypeReference,       'data',                                                             ''],
			
			#[ numberJvmTypeReference,           numberJvmTypeReference,           'data',                                                             ''],
			#[ stringJvmTypeReference,           stringJvmTypeReference,           'data',                                                             ''],
			#[ jsonElementJvmTypeReference,      jsonElementJvmTypeReference,      'data',                                                             ''],
			#[ bigDecimalJvmTypeReference,       bigDecimalJvmTypeReference,       'data',                                                             ''],
			
			#[ enumJvmTypeReference,             enumJvmTypeReference,             'data',                                                             ''],
			
			// coercion from json (always true, access is done through expected type)
			#[ booleanObjectJvmTypeReference,    jsonElementJvmTypeReference,      'data.getAsJsonPrimitive().getAsBoolean()',                         'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isBoolean());'],
			#[ booleanPrimitiveJvmTypeReference, jsonElementJvmTypeReference,      'data.getAsJsonPrimitive().getAsBoolean()',                         'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isBoolean());'],
			#[ intObjectJvmTypeReference,        jsonElementJvmTypeReference,      'data.getAsJsonPrimitive().getAsInt()',                             'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ intPrimitiveJvmTypeReference,     jsonElementJvmTypeReference,      'data.getAsJsonPrimitive().getAsInt()',                             'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ longObjectJvmTypeReference,       jsonElementJvmTypeReference,      'data.getAsJsonPrimitive().getAsLong()',                            'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ longPrimitiveJvmTypeReference,    jsonElementJvmTypeReference,      'data.getAsJsonPrimitive().getAsLong()',                            'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ bigDecimalJvmTypeReference,       jsonElementJvmTypeReference,      'data.getAsJsonPrimitive().getAsBigDecimal()',                      'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ numberJvmTypeReference,           jsonElementJvmTypeReference,      'data.getAsJsonPrimitive().getAsNumber()',                          'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isNumber());'],
			#[ stringJvmTypeReference,           jsonElementJvmTypeReference,      'data.getAsJsonPrimitive().getAsString()',                          'org.junit.Assert.assertTrue("msg", data.getAsJsonPrimitive().isString());'],
			#[ enumJvmTypeReference,             jsonElementJvmTypeReference,      'java.lang.Enum.valueOf(data.getAsJsonPrimitive().getAsString())',  'try { java.lang.Enum.valueOf(data.getAsJsonPrimitive().getAsString()); } catch (IllegalArgumentException ia) { org.junit.Assert.fail("msg"); }' ],
			
			// coercion to string (always true)
			#[ stringJvmTypeReference,           booleanObjectJvmTypeReference,    'Boolean.toString(data)',                                           ''],
			#[ stringJvmTypeReference,           booleanPrimitiveJvmTypeReference, 'Boolean.toString(data)',                                           ''],
			#[ stringJvmTypeReference,           longObjectJvmTypeReference,       'Long.toString(data)',                                              ''],
			#[ stringJvmTypeReference,           longPrimitiveJvmTypeReference,    'Long.toString(data)',                                              ''],
			#[ stringJvmTypeReference,           intObjectJvmTypeReference,        'Integer.toString(data)',                                           ''],
			#[ stringJvmTypeReference,           intPrimitiveJvmTypeReference,     'Integer.toString(data)',                                           ''],
			#[ stringJvmTypeReference,           bigDecimalJvmTypeReference,       'data.toString()',                                                  ''],
			#[ stringJvmTypeReference,           numberJvmTypeReference,           'String.valueOf(data)',                                             ''],
			#[ stringJvmTypeReference,           enumJvmTypeReference,             'data.toString()',                                                  ''],

			// coercion to json (needs parsing)
			#[ jsonElementJvmTypeReference,      booleanObjectJvmTypeReference,    'new com.google.gson.JsonParser().parse(Boolean.toString(data))',   ''],
			#[ jsonElementJvmTypeReference,      booleanPrimitiveJvmTypeReference, 'new com.google.gson.JsonParser().parse(Boolean.toString(data))',   ''],
			#[ jsonElementJvmTypeReference,      intObjectJvmTypeReference,        'new com.google.gson.JsonParser().parse(Integer.toString(data))',   ''],
			#[ jsonElementJvmTypeReference,      intPrimitiveJvmTypeReference,     'new com.google.gson.JsonParser().parse(Integer.toString(data))',   ''],
			#[ jsonElementJvmTypeReference,      longObjectJvmTypeReference,       'new com.google.gson.JsonParser().parse(Long.toString(data))',      ''],
			#[ jsonElementJvmTypeReference,      longPrimitiveJvmTypeReference,    'new com.google.gson.JsonParser().parse(Long.toString(data))',      ''],
			#[ jsonElementJvmTypeReference,      numberJvmTypeReference,           'new com.google.gson.JsonParser().parse(data)',                     ''],
			#[ jsonElementJvmTypeReference,      stringJvmTypeReference,           'new com.google.gson.JsonParser().parse("\\""+data+"\\"")',         ''],
			#[ jsonElementJvmTypeReference,      bigDecimalJvmTypeReference,       'new com.google.gson.JsonParser().parse(data.toString())',          ''],
			#[ jsonElementJvmTypeReference,      enumJvmTypeReference,             'new com.google.gson.JsonParser().parse("\\""+data.toString()+"\\"")', ''],
			
			// coercion from string (always true, needs parsing though)
			#[ booleanObjectJvmTypeReference,    stringJvmTypeReference,           'Boolean.valueOf(data)',                                            'org.junit.Assert.assertTrue("msg", Boolean.TRUE.toString().equals(data) || Boolean.FALSE.toString().equals(data));'],
			#[ booleanPrimitiveJvmTypeReference, stringJvmTypeReference,           'Boolean.valueOf(data)',                                            'org.junit.Assert.assertTrue("msg", Boolean.TRUE.toString().equals(data) || Boolean.FALSE.toString().equals(data));'],
			#[ intObjectJvmTypeReference,        stringJvmTypeReference,           'Integer.parseInt(data)',                                           'try { Integer.parseInt(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			#[ intPrimitiveJvmTypeReference,     stringJvmTypeReference,           'Integer.parseInt(data)',                                           'try { Integer.parseInt(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			#[ longObjectJvmTypeReference,       stringJvmTypeReference,           'Long.parseLong(data)',                                             'try { Long.parseLong(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			#[ longPrimitiveJvmTypeReference,    stringJvmTypeReference,           'Long.parseLong(data)',                                             'try { Long.parseLong(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			#[ bigDecimalJvmTypeReference,       stringJvmTypeReference,           'new java.math.BigDecimal(data)',                                   'try { new java.math.BigDecimal(data); } catch (NumberFormatException nfe) { org.junit.Assert.fail("msg"); }'],
			#[ numberJvmTypeReference,           stringJvmTypeReference,           'java.text.NumberFormat.getInstance().parse(data)',                 'try { java.text.NumberFormat.getInstance().parse(data); } catch (java.text.ParseException pe) { org.junit.Assert.fail("msg"); }'],
			#[ enumJvmTypeReference,             stringJvmTypeReference,           'java.lang.Enum.valueOf(data)',                                     'try { java.lang.Enum.valueOf(data); } catch (IllegalArgumentException ia) { org.junit.Assert.fail("msg"); }'],
			
			// coercion int <- numeric type
			#[ intObjectJvmTypeReference,        longObjectJvmTypeReference,       'java.lang.Math.toIntExact(data)',                                  'try { java.lang.Math.toIntExact(data); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ intPrimitiveJvmTypeReference,     longObjectJvmTypeReference,       'java.lang.Math.toIntExact(data)',                                  'try { java.lang.Math.toIntExact(data); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ intObjectJvmTypeReference,        longPrimitiveJvmTypeReference,    'java.lang.Math.toIntExact(data)',                                  'try { java.lang.Math.toIntExact(data); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ intPrimitiveJvmTypeReference,     longPrimitiveJvmTypeReference,    'java.lang.Math.toIntExact(data)',                                  'try { java.lang.Math.toIntExact(data); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ intObjectJvmTypeReference,        bigDecimalJvmTypeReference,       'data.intValueExact()',                                             'try { data.intValueExact(); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ intPrimitiveJvmTypeReference,     bigDecimalJvmTypeReference,       'data.intValueExact()',                                             'try { data.intValueExact(); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ intObjectJvmTypeReference,        numberJvmTypeReference,           'data',                                                             ''],
			#[ intPrimitiveJvmTypeReference,     numberJvmTypeReference,           'data',                                                             ''],
			
			// coercion long <- numeric type
			#[ longObjectJvmTypeReference,       intObjectJvmTypeReference,        'data',                                                             ''],
			#[ longPrimitiveJvmTypeReference,    intObjectJvmTypeReference,        'data',                                                             ''],
			#[ longObjectJvmTypeReference,       intPrimitiveJvmTypeReference,     'data',                                                             ''],
			#[ longPrimitiveJvmTypeReference,    intPrimitiveJvmTypeReference,     'data',                                                             ''],
			#[ longObjectJvmTypeReference,       bigDecimalJvmTypeReference,       'data.longValueExact()',                                            'try { data.longValueExact(); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ longPrimitiveJvmTypeReference,    bigDecimalJvmTypeReference,       'data.longValueExact()',                                            'try { data.longValueExact(); } catch (ArithmeticException ae) { org.junit.Assert.fail("msg"); }'],
			#[ longObjectJvmTypeReference,       numberJvmTypeReference,           'data',                                                             ''],
			#[ longPrimitiveJvmTypeReference,    numberJvmTypeReference,           'data',                                                             ''],

			// coercion bigDecimal <- numeric type
			#[ bigDecimalJvmTypeReference,       intPrimitiveJvmTypeReference,     'new java.math.BigDecimal(data)',                                   ''],
			#[ bigDecimalJvmTypeReference,       intObjectJvmTypeReference,        'new java.math.BigDecimal(data)',                                   ''],
			#[ bigDecimalJvmTypeReference,       longPrimitiveJvmTypeReference,    'new java.math.BigDecimal(data)',                                   ''],
			#[ bigDecimalJvmTypeReference,       longObjectJvmTypeReference,       'new java.math.BigDecimal(data)',                                   ''],
			#[ bigDecimalJvmTypeReference,       numberJvmTypeReference,           'new java.math.BigDecimal(data)',                                   ''],
 
			// coercion number <- numeric type
			#[ numberJvmTypeReference,           intPrimitiveJvmTypeReference,     'data',                                                             ''],
			#[ numberJvmTypeReference,           intObjectJvmTypeReference,        'data',                                                             ''],
			#[ numberJvmTypeReference,           longPrimitiveJvmTypeReference,    'data',                                                             ''],
			#[ numberJvmTypeReference,           longObjectJvmTypeReference,       'data',                                                             ''],
			#[ numberJvmTypeReference,           bigDecimalJvmTypeReference,       'data',                                                             '']
		]
	}

	@Before
	def void initCoercionComputer() {
		coercionComputer.initWith(null as ResourceSet) // null is allowable for tests but has some restrictions (assignable does not work as it should)
		typeReferenceUtil.initWith(null as ResourceSet)
	}
	
	@Test
	def void testIllegalCoercionImpossible() {
		// not using junit parameters, since I need access on non static injected objects
		illegalCoercionTypePairs.forEach [
			// given
			val target = first 
			val source = second

			// when
			val result = coercionComputer.isCoercionPossible(target, source)

			// then
			assertFalse(result, '''Coercible should be impossible for targetType = '«target?.qualifiedName»' and sourceType = '«source?.qualifiedName»'. ''')
		]
	}
	
	@Test
	def void testIllegalCoercion() {
		// not using junit parameters, since I need access on non static injected objects
		illegalCoercionTypePairs.forEach [
			// given
			val target = first
			val source = second

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
	def void testLegalCoercionIsPossible() {
		// not using junit parameters, since I need access on non static injected objects
		legalCoercionData.forEach [
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
	def void testCoercion() {
		// not using junit parameters, since I need access on non static injected objects
		legalCoercionData.forEach [
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
	def void testCoercionGuard() {
		// not using junit parameters, since I need access on non static injected objects
		legalCoercionData.forEach [
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
	
	private def boolean typeNameEquals(JvmTypeReference typeRefA, Object typeRefBUntyped) {
		val typeRefB = typeRefBUntyped as JvmTypeReference
		return typeRefB.qualifiedName == typeRefA.qualifiedName
	}
	
	@Test
	def void testThatAllPossibleCombinationsAreHeededWithinTestData() {
		// given
		getAllPairs(allKnownTypes, allKnownTypes).forEach [ pair |

			// when (not)
			if (!(legalCoercionData.exists[pair.first.typeNameEquals(get(0)) && pair.second.typeNameEquals(get(1))] ||
				illegalCoercionTypePairs.exists[pair.first.typeNameEquals(first) && pair.second.typeNameEquals(second)])) {

				// then
				fail('''Coercion pair (target = '«pair.first.qualifiedName»', source = '«pair.second.qualifiedName»') is not covered by tests (or rather test data).''')
			}
		]
	}
	
	@Test
	def void testThatValueCoercionsAreExecuted() {
		validValueFromStringCoercion.forEach [ testEntry |
			// given
			val value = testEntry.get(1) as String
			val targetType = testEntry.get(0) as JvmTypeReference

			// when
			val coercible = coercionComputer.isCoercionPossible(targetType, stringJvmTypeReference, value)

			// then
			coercible.assertTrue('''failing to coerce value = '«value»' to targetType = '«targetType.qualifiedName»'  ''')
		]
	}

	@Test
	def void testThatInvalidValueCoercionsAreRejected() {
		invalidValueFromStringCoercion.forEach [ testEntry |
			// given
			val value = testEntry.get(1) as String
			val targetType = testEntry.get(0) as JvmTypeReference

			// when
			val coercible = coercionComputer.isCoercionPossible(targetType, stringJvmTypeReference, value)

			// then
			coercible.assertFalse('''Coercion must fail from value = '«value»' to targetType = '«targetType.qualifiedName»'  ''')
		]
	}

	@Test
	def void validateThatAllKnownTypesAreCheckedForValueCoercion() {
		allKnownTypes.forEach[ typeRef |
			// enum is checked via DummyEnum and can thus not be checked by type name equality
			val found = validValueFromStringCoercion.map[get(0)].filter(JvmTypeReference).exists[typeNameEquals(typeRef) || (typeRef.isEnum && isEnum)]

			found.assertTrue('''known type = '«typeRef.qualifiedName»' was not found in validValueFromStringCoercion tests.''')
		]
	}
	
	@Test
	def void validateThatAllKnownTypesAreCheckedForInvalidValueCoercion() {
		allKnownTypes.forEach[ typeRef |
			// string itself is not tested to have a invalid coercion, since it is not coerced
			// enum is checked via DummyEnum and can thus not be checked by type name equality
			val found = invalidValueFromStringCoercion.map[get(0)].filter(JvmTypeReference).exists[typeNameEquals(typeRef) || (typeRef.isEnum && isEnum)]|| typeRef.isString

			found.assertTrue('''known type = '«typeRef.qualifiedName»' was not found in invalidValueFromStringCoercion tests.''')
		]
	}
	
	@Test
	def void testThatAllCombinationsAreEitherCoercibleOrNonCoercible() {
		// ------------- given
		getAllPairs(allKnownTypes, allKnownTypes).forEach [ pair |
			val typeA = pair.first
			val typeB = pair.second
			
			// ------------- when 
			val coercible = coercionComputer.isCoercionPossible(typeA, typeB)
			if (coercible) {
				try {
					// ------------- then coercion must be possible 
					val coercion = coercionComputer.generateCoercion(typeA, typeB, 'some')
					coercion.assertNotNull
				} catch (Exception e) {
					fail('''Exception during coercion of a combination which was regarded possible (from='«typeB.qualifiedName»', to='«typeA.qualifiedName»').''')
				}
				try {
					// ------------- and coercion guard must yield something
					val guard = coercionComputer.generateCoercionGuard(typeA, typeB, 'some', 'other')
					guard.assertNotNull
				} catch (Exception e) {
					fail('''Exception during coercion guard generation of a combination which was regarded possible (from='«typeB.qualifiedName»', to='«typeA.qualifiedName»').''')
				}
			} else {
				try {
					// ------------- else coercion must fail
					coercionComputer.generateCoercion(typeA, typeB, 'some')
					fail('''Should run into an exception since coercion from = '«typeB.qualifiedName»' to '«typeA.qualifiedName»' is deemed impossible.'''.toString)
				} catch (Exception e) {
					// ignore, since this is ok
				}
				try {
					// ------------- and coercion guard must not be generated
					coercionComputer.generateCoercionGuard(typeA, typeB, 'some', 'other')
					fail('''Should run into an exception since coercion guard from = '«typeB.qualifiedName»' to '«typeA.qualifiedName»' is deemed impossible.'''.toString)
				} catch (Exception e) {
					// ignore, since this is ok
				}
			}
		]
	}
	
	/** 
	 * Get all pairs of elements of these two lists.
	 * 
	 * for all a,b: a element listA, b element listB, yield Pair(a,b)
	 * 
	 * keep in mind that this are not all possible permutations, since no Pairs are generated that have elements
	 * of listB as first element in the pair (and likewise, no Pairs are generated that have elements of listB
	 * as second element in the pair).
	 */
	private def <T, U> Iterable<Pair<T, U>> getAllPairs(Iterable<T> iterableA, Iterable<U> iterableB) {
		return iterableA.map [ a |
			iterableB.map [ b |
				Tuples.create(a, b)
			]
		].flatten
	} 
	
}
