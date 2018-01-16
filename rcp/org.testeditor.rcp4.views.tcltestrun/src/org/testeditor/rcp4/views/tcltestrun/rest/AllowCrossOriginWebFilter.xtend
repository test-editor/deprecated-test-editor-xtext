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

import javax.servlet.Filter
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.FilterChain
import java.io.IOException
import javax.servlet.ServletException
import javax.servlet.FilterConfig
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

class AllowCrossOriginWebFilter implements Filter {

	static val ORIGIN_HEADER = "Origin"
	static val ACCESS_CONTROL_ALLOW_ORIGIN_HEADER = "Access-Control-Allow-Origin"
	static val ACCESS_CONTROL_ALLOW_METHODS_HEADER = "Access-Control-Allow-Methods"

	override destroy() {
		// nothing to do here
	}

	override doFilter(ServletRequest request, ServletResponse response,
		FilterChain chain) throws IOException, ServletException {
		handle(request as HttpServletRequest, response as HttpServletResponse)
		chain.doFilter(request, response)
	}

	def void handle(HttpServletRequest request, HttpServletResponse response) {
		val origin = request.getHeader(ORIGIN_HEADER)
		response.setHeader(ACCESS_CONTROL_ALLOW_ORIGIN_HEADER, origin)
		response.setHeader(ACCESS_CONTROL_ALLOW_METHODS_HEADER, "GET, POST, DELETE")
	}

	override init(FilterConfig filterConfig) throws ServletException {
		// nothing to do here
	}

}
