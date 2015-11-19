package org.testeditor.tsl.dsl.parser

import javax.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import org.testeditor.tsl.dsl.tests.AbstractTest
import org.testeditor.tsl.dsl.tsl.TslModel

abstract class AbstractParserTest extends AbstractTest {

	@Inject protected ParseHelper<TslModel> parser

	protected def TslModel parse(CharSequence input) {
		return parser.parse(input)
	}

}