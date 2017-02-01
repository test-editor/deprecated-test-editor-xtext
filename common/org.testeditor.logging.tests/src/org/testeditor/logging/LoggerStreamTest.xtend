package org.testeditor.logging

import java.io.OutputStream
import org.junit.Before
import org.junit.Test
import org.mockito.Mock
import org.mockito.MockitoAnnotations
import org.slf4j.Logger

import static org.mockito.Matchers.*

import static extension org.mockito.Mockito.*

class LoggerStreamTest {

	var LoggerStream classUnderTest
	@Mock var Logger logger
	@Mock var OutputStream outputStream

	@Before
	def void setUp() {
		MockitoAnnotations.initMocks(this)
		classUnderTest = new LoggerStream(logger, outputStream)
	}

	@Test
	def void testNoNewlineNoLog() {
		// given
		val hello = "Hello".bytes

		// when		
		classUnderTest.write(hello)

		// then
		logger.verify(never).debug(any)
		outputStream.verify.write(hello)
	}

	@Test
	def void testNewlineLogs() {
		// given
		val hello = "Hello\r".bytes

		// when		
		classUnderTest.write(hello)

		// then
		logger.verify.debug("Hello")
		outputStream.verify.write(hello)
	}

	@Test
	def void testNewlineWithPreceedingLogs() {
		// given
		val world = "World, ".bytes
		val hello = "Hello\r".bytes

		// when		
		classUnderTest.write(world)
		classUnderTest.write(hello)

		// then
		logger.verify.debug("World, Hello")
		outputStream.verify.write(world)
		outputStream.verify.write(hello)
	}
	
	@Test
	def void testNewLineAtStart() {
		// given
		val hello = "\rHello".bytes
		
		// when
		classUnderTest.write(hello)

		// then
		logger.verify(never).debug(any)
		outputStream.verify.write(hello)
	}
	
	@Test
	def void testNewLineInTheMiddle() {
		// given
		val hello = "Hello\r World!".bytes
		
		// when
		classUnderTest.write(hello)

		// then
		logger.verify.debug("Hello")
		outputStream.verify.write(hello)
	}

}
