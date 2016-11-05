/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
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
package org.testeditor.rcp4.views.tcltestrun.rest

import java.nio.file.Files
import javax.json.Json
import javax.ws.rs.GET
import javax.ws.rs.Path
import javax.ws.rs.PathParam
import javax.ws.rs.Produces
import javax.ws.rs.core.MediaType
import javax.ws.rs.core.Response
import org.eclipse.xtend.lib.annotations.Accessors
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionManager
import javax.json.JsonObjectBuilder
import java.util.List
import org.testeditor.rcp4.views.tcltestrun.model.TestLogGroupBuilder
import org.testeditor.rcp4.views.tcltestrun.model.TestLogGroup
import org.testeditor.rcp4.views.tcltestrun.model.TestLogGroupComposite
import javax.json.JsonArray

@Path("/testexeclogs")
class TestExecutionLogService {

	@Accessors
	TestExecutionManager testExecutionManager

	@GET
	@Produces(MediaType::APPLICATION_JSON)
	def Response getTestLogExeutionsList() {
		val result = Json.createObjectBuilder
		val array = Json.createArrayBuilder
		testExecutionManager.testExecutionLogs.forEach [
			val execLog = Json.createObjectBuilder
			execLog.add("filename", it.logFile.name)
			execLog.add("name", it.testExecutionName)
			execLog.add("links", createLinks(it.logFile.name))
			array.add(execLog)
		]
		result.add("entries", array)
		return Response.ok(result.build.toString).build
	}

	def JsonArray createLinks(String fileName) {
		val links = Json.createArrayBuilder
		links.add(Json.createObjectBuilder.add("href", '''/testexeclogs/«fileName»/fulllogs''').add("rel", "fullogs"))
		links.add(
			Json.createObjectBuilder.add("href", '''/testexeclogs/«fileName»/logGroups''').add("rel", "logGroups"))
		links.add(Json.createObjectBuilder.add("href", '''/testexeclogs/«fileName»/logGroups''').add("rel", "self"))
		return links.build
	}

	@Path("/{filename}/fulllogs")
	@GET
	@Produces(MediaType::APPLICATION_JSON)
	def Response getTestLogExeutionContent(@PathParam("filename") String filename) {
		val result = Json.createObjectBuilder
		val log = testExecutionManager.testExecutionLogs.filter[logFile.name == filename].head
		result.add("content", Files.readAllLines(log.logFile.toPath).join)
		result.add("links", createLinks(filename))
		return Response.ok(result.build.toString).build
	}

	@Path("/{filename}/logGroups")
	@GET
	@Produces(MediaType::APPLICATION_JSON)
	def Response getTestLogExeutionTestStepTree(@PathParam("filename") String filename) {
		val log = testExecutionManager.testExecutionLogs.filter[logFile.name == filename].head
		val json = createLogGroupJsonArray(Files.readAllLines(log.logFile.toPath))
		json.add("links", createLinks(filename))
		return Response.ok(json.build.toString).build
	}

	def JsonObjectBuilder createLogGroupJsonArray(List<String> logLines) {
		val logGroups = new TestLogGroupBuilder().build(logLines)
		val json = Json.createObjectBuilder
		val arrayBuilder = Json.createArrayBuilder
		logGroups.forEach[arrayBuilder.add(createJsonFrom(it))]
		json.add("logGroups", arrayBuilder)
		return json
	}

	def JsonObjectBuilder createJsonFrom(TestLogGroup group) {
		val json = Json.createObjectBuilder
		json.add("type", group.type.type)
		val arrayBuilder = Json.createArrayBuilder
		group.logLines.forEach[arrayBuilder.add(it)]
		json.add("loglines", arrayBuilder)
		if (group.name != null) {
			json.add("name", group.name)
		}
		if (group instanceof TestLogGroupComposite) {
			val childArrayBuilder = Json.createArrayBuilder
			group.children.forEach[childArrayBuilder.add(createJsonFrom(it))]
			json.add("childs", childArrayBuilder)
		}
		return json
	}

}
