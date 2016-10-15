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

import javax.servlet.Filter
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.FilterChain
import java.io.IOException
import javax.servlet.ServletException
import javax.servlet.FilterConfig
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

class TestEditorWebFilter implements Filter {
	
	private static val ORIGIN_HEADER = "Origin"
	private static val ACCESS_CONTROL_ALLOW_ORIGIN_HEADER = "Access-Control-Allow-Origin"
	
	override destroy() {
	}
	
	override doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
		handle( request as HttpServletRequest,  response as HttpServletResponse)
		chain.doFilter(request, response)
	}
	
	def handle(HttpServletRequest request, HttpServletResponse response) {
		val origin = request.getHeader(ORIGIN_HEADER)
		response.setHeader(ACCESS_CONTROL_ALLOW_ORIGIN_HEADER, origin)
	}
	
	override init(FilterConfig filterConfig) throws ServletException {
	}
	
}