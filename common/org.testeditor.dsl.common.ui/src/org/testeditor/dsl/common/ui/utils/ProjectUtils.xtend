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

import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.preferences.IEclipsePreferences
import org.eclipse.core.runtime.preferences.InstanceScope

class ProjectUtils {


	public static val String BASIC_PATH_PATTERN = "[^/]+(/[^/]+)*"

	/**
	 * get a folder following the relative path given (which may include slashes)
	 * @param project Project within to operate
	 * @param path Path of the form dir/dir/dir
	 */
	def IFolder getDeepFolder(IProject project, String path) {
		return createOrGetDeepFolder(project, path, false)
	}

	/**
	 * get a folder following the relative path given (which may include slashes)
	 * and create it (and all folders in between) if not existent
	 * @param project Project within to operate
	 * @param path Path of the form dir/dir/dir
	 */
	def IFolder createOrGetDeepFolder(IProject project, String path) {
		return createOrGetDeepFolder(project, path, true)
	}

	/**
	 * get a folder following the relative path given (which may include slashes)
	 * and create it (and all folders in between) if wanted and not existent
	 * @param project Project within to operate
	 * @param path Path of the form dir/dir/dir
	 * @param create true = create if non existent, false = don't create, yield null if non existent
	 */
	private def IFolder createOrGetDeepFolder(IProject project, String path, boolean create) {
		if (!path.matches(BASIC_PATH_PATTERN)) {
			throw new RuntimeException('''Path "«path»" does not match expected pattern «BASIC_PATH_PATTERN»''')
		}
		val srcFolders = path.split('/')
		var IFolder folder = null
		for (var i = 0; i < srcFolders.length; i++) {
			folder = if (i == 0) {
				project.getFolder(srcFolders.get(i))
			} else {
				folder.getFolder(srcFolders.get(i))
			}
			if (!folder.exists) {
				if (create) {
					folder.create(true, false, null)
				} else {
					return null
				}
			}
		}
		return folder
	}

	/** get a preference node (e.g. for node "org.testeditor.tcl.dsl.Tcl") */
	def IEclipsePreferences getPrefsNode(String node) {
		return InstanceScope.INSTANCE.getNode(node);
	}

	/** persist (changed) preferences */
	def void save(IEclipsePreferences prefs) {
		prefs.flush
	}
	
}
