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

import org.eclipse.ui.plugin.AbstractUIPlugin
import org.osgi.framework.BundleContext

class Activator extends AbstractUIPlugin {
	// $NON-NLS-1$
	// The shared instance
	private static var Activator plugin

	/**
	 * The constructor
	 */
	new() {
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see AbstractUIPlugin#start(org.osgi.framework.
	 * BundleContext)
	 */
	override void start(BundleContext context) throws Exception {
		super.start(context)
		plugin = this
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see AbstractUIPlugin#stop(org.osgi.framework.
	 * BundleContext)
	 */
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
