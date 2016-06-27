package org.testeditor.tml.dsl.validation

import javax.inject.Inject
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.junit.Before
import org.mockito.ArgumentCaptor
import org.mockito.Captor
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.tml.dsl.tests.parser.AbstractParserTest
import org.testeditor.tml.util.TmlModelUtil
import org.testeditor.tml.dsl.tests.TmlModelGenerator

abstract class AbstractTmlValidatorTest extends AbstractParserTest {
	@InjectMocks protected TmlValidator tmlValidator // class under test
	@Mock protected TmlModelUtil tmlModelUtil // injected into class under test
	@Mock protected ValidationMessageAcceptor messageAcceptor

	@Captor protected ArgumentCaptor<String> message

	@Inject protected extension TmlModelGenerator
	@Inject protected extension TmlModelGeneratorForValidation

	@Before
	def void initMessageAcceptor() {
		val state = tmlValidator.setMessageAcceptor(messageAcceptor)
		state.state // needs to be called in order for internal state to be initialized. this again is necessary to allow messages to be issued on the "currentObject" of the validation
	}

}
