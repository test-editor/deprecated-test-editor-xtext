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
package org.testeditor.rcp4.views.tcltestrun.rest

import org.osgi.service.http.HttpContext
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import java.io.IOException
import org.osgi.framework.Bundle

class BundleHttpContext implements HttpContext {

	Bundle bundle
	
	new(Bundle bundle) {
		this.bundle = bundle
	}
	
	override getMimeType(String name) {
		return null
	}

	override getResource(String name) {
		if (name !== null) {
			if (name.startsWith("/")) {
				return bundle.getEntry(name.substring(1))
			}
			return bundle.getEntry(name)
		}
		return null
	}

	override handleSecurity(HttpServletRequest request, HttpServletResponse response) throws IOException {
		return true
	}

}
