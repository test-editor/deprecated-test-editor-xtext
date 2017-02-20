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
package org.testeditor.rcp4.views.tcltestrun

import org.eclipse.e4.core.contexts.ContextInjectionFactory
import org.eclipse.e4.core.contexts.EclipseContextFactory
import org.eclipse.equinox.http.servlet.ExtendedHttpService
import org.eclipse.ui.plugin.AbstractUIPlugin
import org.osgi.framework.BundleContext
import org.testeditor.rcp4.views.tcltestrun.model.TestExecutionManager
import org.testeditor.rcp4.views.tcltestrun.rest.AllowCrossOriginWebFilter
import org.testeditor.rcp4.views.tcltestrun.rest.BundleHttpContext
import org.testeditor.rcp4.views.tcltestrun.rest.TestExecutionLogService

class Activator extends AbstractUIPlugin {

	// The shared instance
	private static var Activator plugin

	override void start(BundleContext context) throws Exception {
		super.start(context)
		plugin = this
		registerServices(context)
	}

	private def void registerServices(BundleContext context) {
		// Register TestExecutionLogService
		val eclipseContext = EclipseContextFactory.create
		val restService = ContextInjectionFactory.make(TestExecutionLogService, eclipseContext)
		restService.testExecutionManager = ContextInjectionFactory.make(TestExecutionManager, eclipseContext)
		context.registerService(TestExecutionLogService, restService, null)

		// Register TestEditorWebFilter
		val serviceReference = context.getServiceReference(ExtendedHttpService)
		if (serviceReference !== null) {
			val httpService = context.getService(serviceReference)
			httpService.registerResources("/", "/web-app", new BundleHttpContext(bundle))
			httpService.registerFilter("/services/*", new AllowCrossOriginWebFilter, null, null)
		}
	}

	override void stop(BundleContext context) throws Exception {
		plugin = null
		super.stop(context)
	}

	/**
	 * Returns the shared instance
	 * @return the shared instance
	 */
	def static Activator getDefault() {
		return plugin
	}

}
