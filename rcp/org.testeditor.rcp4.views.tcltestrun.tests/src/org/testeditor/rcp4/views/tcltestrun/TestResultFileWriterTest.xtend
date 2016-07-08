package org.testeditor.rcp4.views.tcltestrun

import org.mockito.runners.MockitoJUnitRunner
import org.junit.runner.RunWith
import org.mockito.InjectMocks
import org.junit.Test
import java.io.File
import java.nio.file.Files

import static org.junit.Assert.*

@RunWith(MockitoJUnitRunner)
class TestResultFileWriterTest {

	@InjectMocks
	TestResultFileWriter testResultWriter

	@Test
	def void testErrorFile() {
		val errorFile = File.createTempFile("mFile", "tmp")
		testResultWriter.writeErrorFile("MyPrj.package.MyTest", errorFile)
		val lines = Files.readAllLines(errorFile.toPath)
		var found = false
		for (line : lines) {
			if(line.contains("MyPrj.package.MyTest")) {
				found = true
			}
		}
		assertTrue(found)
	}
}
