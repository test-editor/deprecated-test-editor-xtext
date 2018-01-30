/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.rcp4.views.tcltestrun

import java.io.File
import java.io.IOException
import java.nio.charset.StandardCharsets
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Attr
import org.w3c.dom.Document
import org.w3c.dom.Node
import com.google.common.io.Files
import org.slf4j.LoggerFactory

class TestResultFileWriter {
	static val logger = LoggerFactory.getLogger(TestResultFileWriter)

	def void writeTestResultFile(String projectName, File resultFile, File[] xmlResults) {
		val docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder
		val resultDoc = docBuilder.newDocument
		val testRun = resultDoc.createElement("testrun")
		resultDoc.appendChild(testRun)
		var testCount = 0;
		var failureCount = 0;
		var errorsCount = 0;
		var ignoreCount = 0;
		for (file : xmlResults) {
			val suiteDoc = docBuilder.parse(file)
			val nodeList = suiteDoc.childNodes
			for (var i = 0; i < nodeList.length; i++) {
				if (nodeList.item(i).nodeName.equals("testsuite")) {
					testRun.appendChild(resultDoc.importNode(nodeList.item(i), true))
					testCount += getIntFromAttribute(nodeList.item(i), "tests")
					failureCount += getIntFromAttribute(nodeList.item(i), "failures")
					errorsCount += getIntFromAttribute(nodeList.item(i), "errors")
					ignoreCount += getIntFromAttribute(nodeList.item(i), "skipped")
				}
			}
		}
		testRun.attributeNode = resultDoc.createAttribute("name", "java")
		testRun.attributeNode = resultDoc.createAttribute("project", projectName)
		testRun.attributeNode = resultDoc.createAttribute("tests", Integer.toString(testCount))
		testRun.attributeNode = resultDoc.createAttribute("started", Integer.toString(testCount))
		testRun.attributeNode = resultDoc.createAttribute("failures", Integer.toString(failureCount))
		testRun.attributeNode = resultDoc.createAttribute("errors", Integer.toString(errorsCount))
		testRun.attributeNode = resultDoc.createAttribute("ignored", Integer.toString(ignoreCount))
		val transformerFactory = TransformerFactory.newInstance();
		val transformer = transformerFactory.newTransformer();
		val source = new DOMSource(resultDoc);
		transformer.transform(source, new StreamResult(resultFile));
	}

	def int getIntFromAttribute(Node node, String attributeName) {
		Integer.parseInt(node.attributes.getNamedItem(attributeName).nodeValue)
	}

	def Attr createAttribute(Document doc, String name, Object value) {
		var result = doc.createAttribute(name)
		result.value = String.valueOf(value)
		return result
	}

	/**
	 * write a default error file for junit
	 */
	def void writeErrorFile(String elementId, File file) {
		try {
			val content = '''
			<?xml version="1.0" encoding="UTF-8"?>
			<testsuite name="«elementId»" tests="1" skipped="0" failures="0" errors="1" time="0.000">
			  <properties/>
			  <testcase name="execute" classname="«elementId»" time="0.000">
			    <error>
			      failed to execute test, please check your technical test setup 
			    </error>
			  </testcase>
			</testsuite>'''
			file.parentFile.mkdirs
			Files.write(content, file, StandardCharsets.UTF_8);
			logger.error("Error file {} written.",file.path)
		} catch (IOException e) {
			logger.error('''could not write test result error file='«file.path»'.''', e)
		}
	}

}
