package org.testeditor.dsl.common.util

import java.util.Map

class CollectionUtils {

	public static def <K,V> Map<K,V> putIfAbsent(Map<K,V> map, K key, V value){
		if(!map.containsKey(key)){
			map.put(key, value)
		}
		return map
	}

}