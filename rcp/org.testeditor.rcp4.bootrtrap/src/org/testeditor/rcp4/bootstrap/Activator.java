package org.testeditor.rcp4.bootstrap;

import java.net.BindException;
import java.net.ServerSocket;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

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
		int retryCounter = 0;
		boolean portIsInUse = true;
		while (portIsInUse && retryCounter < 10) {
			try {
				retryCounter++;
				ServerSocket socket = new ServerSocket(
						Integer.parseInt(System.getProperty("org.osgi.service.http.port")));
				portIsInUse = false;
				socket.close();
			} catch (BindException e) {
				int newPort = Integer.parseInt(System.getProperty("org.osgi.service.http.port")) + retryCounter;
				System.setProperty("org.osgi.service.http.port", Integer.toString(newPort));
			}
		}
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
