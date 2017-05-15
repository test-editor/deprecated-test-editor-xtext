package org.testeditor.aml.dsl.scoping

import javax.inject.Inject
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.testeditor.aml.dsl.tests.AbstractAmlTest
import org.testeditor.dsl.common.testing.DslParseHelper

class AbstractScopingTest extends AbstractAmlTest {

	@Inject protected extension DslParseHelper
	@Inject protected extension ValidationTestHelper

}
