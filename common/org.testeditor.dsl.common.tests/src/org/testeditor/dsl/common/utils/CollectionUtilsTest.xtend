package org.testeditor.dsl.common.utils

import org.junit.Test
import java.util.HashMap
import  static org.junit.Assert.*

class CollectionUtilsTest {
	
	@Test
	def void testNoAddIfExisting() {
		// given
		val map=new HashMap<String,String>(#{"existing" -> "value"})
		
		// when
		map.putIfAbsent("existing", "othervalue")
		
		// then
		assertEquals("value", map.get("existing"))
		assertEquals(1, map.size)
	}
	
	@Test
	def void testAddIfNotExisting(){
		// given
		val map=new HashMap<String,String>
		
		// when
		map.putIfAbsent("notexisting", "othervalue")
		
		// then
		assertEquals("othervalue", map.get("notexisting"))
		assertEquals(1, map.size)
	}
	
}