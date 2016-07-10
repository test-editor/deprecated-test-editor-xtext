package org.testeditor.rcp4.views.tcltestrun

import com.google.common.io.Files
import java.io.File
import java.nio.charset.StandardCharsets
import javax.xml.parsers.DocumentBuilderFactory
import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.InjectMocks
import org.mockito.runners.MockitoJUnitRunner

import static org.junit.Assert.*

@RunWith(MockitoJUnitRunner)
class TestResultFileWriterTest {

	@InjectMocks
	TestResultFileWriter testResultWriter

	@Test
	def void testErrorFile() {
		// given
		val errorFile = File.createTempFile("mFile", "tmp")
		// when
		testResultWriter.writeErrorFile("MyPrj.package.MyTest", errorFile)
		// then		
		val lines = Files.readLines(errorFile, StandardCharsets.UTF_8)
		var found = false
		for (line : lines) {
			if (line.contains("MyPrj.package.MyTest")) {
				found = true
			}
		}
		assertTrue(found)
	}

	@Test
	def void testCreateAttribute() {
		// given
		val doc = DocumentBuilderFactory.newInstance().newDocumentBuilder.newDocument
		// when
		val attr = testResultWriter.createAttribute(doc, "myAttr", "value")
		// then
		assertEquals("myAttr", attr.nodeName)
		assertEquals("value", attr.nodeValue)
	}

	@Test
	def void testIntFromAttribute() {
		// given
		val doc = DocumentBuilderFactory.newInstance().newDocumentBuilder.newDocument
		val node = doc.createElement("myTag")
		val attr = doc.createAttribute("foo")
		attr.value = "20"
		node.attributeNode = attr
		val attr2 = doc.createAttribute("bar")
		attr2.value = "10"
		node.attributeNode = attr2
		// when
		val count1 = testResultWriter.getIntFromAttribute(node, "foo")
		val count2 = testResultWriter.getIntFromAttribute(node, "bar")
		// then
		assertEquals(20, count1)
		assertEquals(10, count2)
	}

	@Test
	def void testWriteComposedFile() {
		// given
		val test1 = File.createTempFile("result1", "xml")
		Files.write('''
		<?xml version="1.0" encoding="UTF-8"?>
		<testsuite tests="1" failures="0" name="InsuranceWebTest" time="32.736" errors="0" skipped="0">
		  <properties/>
		  <testcase name="execute" classname="InsuranceWebTest" time="32.736">
		  </testcase>
		</testsuite>''', test1, StandardCharsets.UTF_8)
		val test2 = File.createTempFile("result1", "xml")
		Files.write('''
		<?xml version="1.0" encoding="UTF-8"?>
		<testsuite tests="1" failures="0" name="UserInformationWebTest" time="4.164" errors="0" skipped="0">
		  <properties/>
		  <testcase name="execute" classname="UserInformationWebTest" time="4.164">
		  </testcase>
		</testsuite>''', test2, StandardCharsets.UTF_8)
		val resultFile = File.createTempFile("composedResult", "xml")
		// when
		testResultWriter.writeTestResultFile("project", resultFile, #[test1, test2])
		// then
		val doc = DocumentBuilderFactory.newInstance().newDocumentBuilder.parse(resultFile)
		val nodes = doc.getElementsByTagName("testrun")
		assertEquals(1, nodes.length)
		assertEquals("2", nodes.item(0).attributes.getNamedItem("tests").nodeValue)
		assertEquals(2, nodes.item(0).childNodes.length)
	}

}
