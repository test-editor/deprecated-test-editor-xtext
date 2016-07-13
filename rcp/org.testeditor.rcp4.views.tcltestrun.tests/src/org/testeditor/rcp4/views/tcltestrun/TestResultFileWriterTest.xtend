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
import org.junit.Rule
import org.junit.rules.TemporaryFolder

@RunWith(MockitoJUnitRunner)
class TestResultFileWriterTest {

	@InjectMocks TestResultFileWriter testResultWriter
	@Rule public TemporaryFolder tempFolder = new TemporaryFolder();

	@Test
	def void testErrorFile() {
		// given
		val errorFile = File.createTempFile("mFile", "tmp")

		// when
		testResultWriter.writeErrorFile("MyPrj.package.MyTest", errorFile)

		// then		
		val lines = Files.toString(errorFile, StandardCharsets.UTF_8)
		assertTrue(lines.contains("MyPrj.package.MyTest"))
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
		val test1 = tempFolder.newFile("result1.xml")
		val test1Content = '''
		<?xml version="1.0" encoding="UTF-8"?>
		<testsuite tests="1" failures="0" name="InsuranceWebTest" time="32.736" errors="0" skipped="0">
		  <properties/>
		  <testcase name="execute" classname="InsuranceWebTest" time="32.736">
		  </testcase>
		</testsuite>'''
		Files.write(test1Content, test1, StandardCharsets.UTF_8)
		val test2 = tempFolder.newFile("result2.xml")
		val test2Content = '''
		<?xml version="1.0" encoding="UTF-8"?>
		<testsuite tests="1" failures="0" name="UserInformationWebTest" time="4.164" errors="0" skipped="0">
		  <properties/>
		  <testcase name="execute" classname="UserInformationWebTest" time="4.164">
		  </testcase>
		</testsuite>'''
		Files.write(test2Content, test2, StandardCharsets.UTF_8)
		val resultFile = tempFolder.newFile("composedResult.xml")

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
