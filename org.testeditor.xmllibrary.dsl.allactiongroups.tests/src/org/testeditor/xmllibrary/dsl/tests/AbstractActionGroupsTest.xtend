package org.testeditor.xmllibrary.dsl.tests

import com.google.inject.Injector
import javax.inject.Inject
import org.eclipse.xtext.junit4.AbstractXtextTests
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.junit.runner.RunWith
import org.testeditor.xmllibrary.dsl.AllActionGroupsDslInjectorProvider

@InjectWith(AllActionGroupsDslInjectorProvider)
@RunWith(XtextRunner)
abstract class AbstractActionGroupsTest extends AbstractXtextTests {

	@Inject Injector injector

	override setUp() throws Exception {
		super.setUp()
		setInjector(this.injector)
	}

}
