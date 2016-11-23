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
package org.testeditor.rcp4.bootstrap;

import java.net.BindException;
import java.net.ServerSocket;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

	private static final String OSGI_HTTP_PORT = "org.osgi.service.http.port";
	private static BundleContext context;

	static BundleContext getContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.
	 * BundleContext)
	 */
	public void start(BundleContext bundleContext) throws Exception {
		Activator.context = bundleContext;
		if (System.getProperty(OSGI_HTTP_PORT) != null) {
			try {
				ServerSocket socket = new ServerSocket(Integer.parseInt(System.getProperty(OSGI_HTTP_PORT)));
				socket.close();
			} catch (BindException e) {
				ServerSocket socket = new ServerSocket(0);
				System.setProperty(OSGI_HTTP_PORT, Integer.toString(socket.getLocalPort()));
				System.out.println("port: " + socket.getLocalPort());
				socket.close();
			}
		} // else: the property is expected to be set in the testeditor.ini. If
			// someone removes the property we don't set a port.
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		Activator.context = null;
	}

}
