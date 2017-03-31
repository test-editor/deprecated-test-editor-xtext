package org.testeditor.dsl.common.testing

import com.google.inject.Guice
import com.google.inject.Injector
import com.google.inject.Module
import com.google.inject.util.Modules
import java.util.List
import com.google.inject.Inject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.resource.impl.BinaryGrammarResourceFactoryImpl
import org.junit.Before
import org.junit.BeforeClass
import org.mockito.MockitoAnnotations
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.testeditor.dsl.common.testing.xtext.XtextAssertionHelper

/**
 * Abstract dependency-injection aware test class for Xtext tests.
 * We don't want to use the XtextRunner as this limits us to not
 * use e.g. parameterized tests.
 */
abstract class AbstractTest {

	@Inject Injector injector

	@Inject protected extension AssertionHelper
	@Inject protected extension XtextAssertionHelper

	/** Subclass-aware logger. */
	protected extension Logger logger = LoggerFactory.getLogger(getClass())

	@BeforeClass
	static def void registerXtextBin() {
		Resource.Factory.Registry.INSTANCE.extensionToFactoryMap.putIfAbsent("xtextbin", new BinaryGrammarResourceFactoryImpl)
	}

	@Before
	def void performInjection() {
		MockitoAnnotations.initMocks(this)
		if (injector === null) {
			injector = createInjector
			injector.injectMembers(this)
		} // else: already injection aware
	}

	protected def Injector createInjector() {
		val modules = newLinkedList()
		modules.collectModules
		return Guice.createInjector(modules.mixin)
	}

	protected def void collectModules(List<Module> modules) {
	}

	/**
	 * Copied from org.eclipse.xtext.util.Modules2
	 */
	protected static def Module mixin(Module... modules) {
		if (modules.length == 0) {
			return Modules.EMPTY_MODULE
		}
		var current = modules.head
		for (var i = 1; i < modules.length; i++) {
			current = Modules.override(current).with(modules.get(i))
		}
		return current
	}

}
