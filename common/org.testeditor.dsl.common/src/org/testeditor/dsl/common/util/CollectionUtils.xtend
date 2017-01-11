package org.testeditor.dsl.common.util

import com.google.common.collect.Iterables

class CollectionUtils {

	/**
	 * filter from the given list of pairs all the ones where the value is (assignable to class) clazz
	 */
	public def <A, B, C> Iterable<Pair<C, A>> filterValue(Iterable<Pair<C, B>> unfiltered, Class<A> clazz) {
		return Iterables.filter(unfiltered, [clazz.isAssignableFrom(value.class)]).map [
			new Pair<C, A>(key, value as A)
		]
	}

	/**
	 * filter from the given list of pairs all the ones where the key is (assignable to class) clazz
	 */
	public def <A, B, C> Iterable<Pair<B, A>> filterKey(Iterable<Pair<C, A>> unfiltered, Class<B> clazz) {
		return Iterables.filter(unfiltered, [clazz.isAssignableFrom(key.class)]).map [
			new Pair<B, A>(key as B, value)
		]
	}

	/**
	 * filter against types (must match one of the given types)
	 */
	public def Iterable<?> filterByTypes(Iterable<?> unfiltered, Class<?> ... types) {
		unfiltered.filter[obj|types.exists[isInstance(obj)]]
	}


}