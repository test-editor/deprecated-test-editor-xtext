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

import javax.json.Json
import javax.ws.rs.GET
import javax.ws.rs.Path
import javax.ws.rs.Produces
import javax.ws.rs.core.MediaType

@Path("/testexeclog")
class TestExecutionLogService {
	
	@Path("/metadata") 	
	@GET 
	@Produces(MediaType::APPLICATION_JSON) 
	def String getTestLogExeutionMetaData() {
		val json = Json.createObjectBuilder
		json.add("value", "Foo")		
		return json.build.toString		
	}
	
}