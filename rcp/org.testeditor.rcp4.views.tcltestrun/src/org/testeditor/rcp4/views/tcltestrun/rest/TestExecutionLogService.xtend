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

@Path("/testexeclogs") 
class TestExecutionLogService {
	
	@Accessors
	TestExecutionManager testExecutionManager
		
	@GET 
	@Produces(MediaType::APPLICATION_JSON) 
	def Response getTestLogExeutionsList() {
		val result = Json.createObjectBuilder
		val array = Json.createArrayBuilder
		testExecutionManager.testExecutionLogs.forEach[
			val execLog = Json.createObjectBuilder
			execLog.add("filename",it.logFile.name)
			execLog.add("name",it.testName)
			execLog.add("href",'''/testexeclogs/«it.logFile.name»/fulllogs''')
			array.add(execLog)
		]
		result.add("entries",array)
		return Response.ok(result.build.toString).build
	}
	
	@Path("/{filename}/fulllogs")
	@GET 
	@Produces(MediaType::APPLICATION_JSON) 
	def Response getTestLogExeutionContent(@PathParam("filename") String filename) {
		val result = Json.createObjectBuilder
		val log = testExecutionManager.testExecutionLogs.filter[logFile.name == filename].head
		result.add("content",Files.readAllLines(log.logFile.toPath).join);
		return Response.ok(result.build.toString).build
	}
	
}