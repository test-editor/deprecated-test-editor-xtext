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
package org.testeditor.dsl.common.ui.utils

import com.google.inject.Guice
import com.google.inject.Injector
import org.eclipse.ui.plugin.AbstractUIPlugin
import org.eclipse.xtend.lib.annotations.Accessors
import org.osgi.framework.BundleContext
import org.slf4j.LoggerFactory

class Activator extends AbstractUIPlugin {

	private static val logger = LoggerFactory.getLogger(Activator)

	@Accessors(PUBLIC_GETTER) private static Activator instance
	@Accessors(PUBLIC_GETTER) private var Injector injector

	override void start(BundleContext context) throws Exception {
		super.start(context)
		instance = this
		try {
			injector = Guice.createInjector
		} catch (Exception e) {
			logger.error(e.getMessage(), e)
			throw e
		}
	}

	override void stop(BundleContext context) throws Exception {
		injector = null
		super.stop(context)
	}

}
