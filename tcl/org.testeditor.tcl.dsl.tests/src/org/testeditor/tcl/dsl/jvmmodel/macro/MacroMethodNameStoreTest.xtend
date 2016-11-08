package org.testeditor.tcl.dsl.jvmmodel.macro

import javax.inject.Inject
import org.junit.Before
import org.junit.Test
import org.testeditor.tcl.Macro
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator

class MacroMethodNameStoreTest extends AbstractTclTest {

	@Inject MacroMethodNameStore nameStore
	@Inject extension TclModelGenerator

	Macro macro1
	Macro macro2
	Macro macro3

	/**
	 * Create a nasty macro collection where the name "macro" is used 3 times.
	 */
	@Before
	def void setupMacroCollection() {
		macro1 = macro('mymacro')
		macro2 = macro('mymacro')
		macro3 = macro('mymacro')
		macroCollection('collection') => [
			macros += macro1
			macros += macro2
			macros += macro3
		]
	}

	@Test
	def void namesAreCreatedProperly() {
		// when
		val proposedName = nameStore.getMethodName(macro1)

		// then
		proposedName.assertEquals('macro_collection_mymacro')
	}

	@Test
	def void duplicateNamesAreAvoided() {
		// when
		val nameForMacro1 = nameStore.getMethodName(macro1)
		val nameForMacro2 = nameStore.getMethodName(macro2)
		val nameForMacro3 = nameStore.getMethodName(macro3)

		// then
		nameForMacro1.assertEquals('macro_collection_mymacro')
		nameForMacro2.assertEquals('macro_collection_mymacro_1')
		nameForMacro3.assertEquals('macro_collection_mymacro_2')
	}

	@Test
	def void repeatedCallsYieldTheSameName() {
		val nameForMacro1First = nameStore.getMethodName(macro1)
		val nameForMacro2First = nameStore.getMethodName(macro2)
		val nameForMacro1Second = nameStore.getMethodName(macro1)
		val nameForMacro2Second = nameStore.getMethodName(macro2)

		// then
		nameForMacro1First.assertEquals('macro_collection_mymacro')
		nameForMacro1Second.assertEquals('macro_collection_mymacro')
		nameForMacro2First.assertEquals('macro_collection_mymacro_1')
		nameForMacro2Second.assertEquals('macro_collection_mymacro_1')
	}

}
