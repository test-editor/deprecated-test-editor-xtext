package org.testeditor.dsl.common.util

import java.math.BigInteger
import javax.inject.Inject
import org.junit.Test
import org.testeditor.dsl.common.testing.AbstractTest

class CollectionUtilsTest extends AbstractTest {

	@Inject extension CollectionUtils

	@Test
	def void testFilterKey() {
		// given
		val Iterable<Pair<Object, String>> iterable = #[
			"x" -> "y",
			1bi -> "one",
			2bi -> "two"
		]

		// when
		val filtered = iterable.filterKey(BigInteger)

		// then
		filtered.assertSize(2) => [
			get(0).assertEquals(iterable.get(1))
			get(1).assertEquals(iterable.get(2))
		]
	}

	@Test
	def void testFilterValue() {
		// given
		val Iterable<Pair<String, Object>> iterable = #[
			"x" -> "y",
			"one" -> 1bi,
			"two" -> 2bi
		]

		// when
		val filtered = iterable.filterValue(BigInteger)

		// then
		filtered.assertSize(2) => [
			get(0).assertEquals(iterable.get(1))
			get(1).assertEquals(iterable.get(2))
		]
	}

}
