package org.testeditor.tcl.dsl.validation

import javax.inject.Inject
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.junit.Before
import org.mockito.ArgumentCaptor
import org.mockito.Captor
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.util.TclModelUtil

abstract class AbstractMockedTclValidatorTest extends AbstractParserTest {

	@InjectMocks protected TclValidator tclValidator // class under test
	@Mock protected TclModelUtil tclModelUtil // injected into class under test
	@Mock protected ValidationMessageAcceptor messageAcceptor

	@Captor protected ArgumentCaptor<String> message

	@Inject protected extension TclModelGenerator tclModelGenerator

	@Before
	def void initMessageAcceptor() {
		val state = tclValidator.setMessageAcceptor(messageAcceptor)
		state.state // needs to be called in order for internal state to be initialized. this again is necessary to allow messages to be issued on the "currentObject" of the validation
	}

}
