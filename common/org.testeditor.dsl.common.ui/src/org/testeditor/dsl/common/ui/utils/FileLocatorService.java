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
 *******************************************************************************/
package org.testeditor.dsl.common.ui.utils;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;

/**
 * Utility service to lookup files in the OSGi context.
 */
public class FileLocatorService {

	/**
	 * Returns if exists the absolute location of a bundle at the file system
	 * related to the given bundle name.
	 * 
	 * @param bundleName
	 *            bundle name (e.g. "org.testeditor.ui")
	 * @return string representation of the absolute path to the bundle
	 * @throws IOException
	 *             is thrown if file-operations failed
	 */
	public String findBundleFileLocationAsString(String bundleName) throws IOException {
		Bundle[] bundles = getBundleContext().getBundles();
		for (Bundle bundle : bundles) {
			if (bundle.getSymbolicName().equals(bundleName)) {
				return FileLocator.getBundleFile(bundle).getAbsolutePath();
			}
		}
		return null;
	}

	/**
	 * Returns the absolute location of a bundle at the file system related to
	 * the given class.
	 * 
	 * @param class1
	 *            class that is member of the bundle to be looked up
	 * @return string representation of the absolute path to the bundle
	 * @throws IOException
	 *             is thrown if file-operations failed
	 */
	public String getBundleLocationFor(Class<?> clazz) throws IOException {
		File bundleFile = FileLocator.getBundleFile(FrameworkUtil.getBundle(clazz));
		return bundleFile.getAbsolutePath();
	}

	/**
	 * Returns the absolute location of a bundle at the file system related to
	 * the given class.
	 * 
	 * @param class1
	 *            class that is member of the bundle to be looked up
	 * @return string representation of the absolute path to the bundle
	 * @throws IOException
	 *             is thrown if file-operations failed
	 */
	public File getBundleFileFor(Class<?> clazz) throws IOException {
		File bundleFile = FileLocator.getBundleFile(FrameworkUtil.getBundle(clazz));
		return bundleFile;
	}

	/**
	 * Returns the bundle context. This implementation doesn't use any reference
	 * to the core activator.
	 * 
	 * @return the bundle context of this class
	 */
	private BundleContext getBundleContext() {
		return FrameworkUtil.getBundle(FileLocatorService.class).getBundleContext();
	}

	/**
	 * Return a file object that points to the workspace of the running
	 * Test-Editor instance.
	 * 
	 * @return file that points to the workspace root.
	 */
	public File getWorkspace() {
		return Platform.getLocation().toFile();
	}

}
