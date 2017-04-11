/*******************************************************************************
 * Copyright (c) 2012 - 2017 Signal Iduna Corporation and others.
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

import com.google.common.collect.Iterables
import java.util.Map
import java.util.Optional

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
	 * filter from the given Map all the ones where the key is (assignable to class) clazz
	 */
	public def <A, B> Map<A, B> filterKey(Map<A, B> unfiltered, Class<? extends A> clazz) {
		return unfiltered.filter[key,value|clazz.isAssignableFrom(key.class)]
	}

	/**
	 * filter against types (must match one of the given types)
	 */
	public def Iterable<?> filterByTypes(Iterable<?> unfiltered, Class<?> ... types) {
		unfiltered.filter[obj|types.exists[isInstance(obj)]]
	}
	
	/**
	 * filter a list of pairs on all values that are not null
	 */
	public def <A, B> Iterable<Pair<A,B>> filterValuesNull(Iterable<Pair<A,B>> unfiltered) {
		return Iterables.filter(unfiltered, [value !== null])
	}
	
	/**
	 * return Iterable exluding the last element
	 */
	public def <T> Iterable<T> butLast(Iterable<T> includingLast) {
		return includingLast.take(includingLast.size-1)
	}

	/**
	 * return the index (0-based) of the first element that satisfies the predicate. -1 if not found.
	 */
	public def <A> int indexOfFirst(Iterable <A> iterable, (A)=>Boolean predicate) {
		return Optional.ofNullable(iterable.indexed.findFirst[predicate.apply(value)]?.key).orElse(-1)
	}

	/**
	 * return the index (0-based) of the first element that is 'identityEquals' to the passed object. -1 if not found.
	 */
	public def <A> int indexOfFirst(Iterable <A> iterable, A actual) {
		return Optional.ofNullable(iterable.indexed.findFirst[value.identityEquals(actual)]?.key).orElse(-1)
	}


	/**
	 * combine two maps into a union (replace duplicates keys with values from right)
	 */
	public def <K,V> Map<K,V> union(Map<K,V> left, Map<K,V> right) {
		return newHashMap => [putAll(left) putAll(right)]
	}
	
	/**
	 * combine maps into a union (duplicates within later maps overwrite previous ones)
	 */
	public def <K,V> Map<K,V> union(Iterable<Map<K,V>> mapList) {
		return mapList.reduce[left,right|union(left,right)]
	}

}