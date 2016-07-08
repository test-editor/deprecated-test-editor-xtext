package org.testeditor.tcl.dsl.ui.testlaunch

import org.eclipse.core.resources.IProject
import org.eclipse.jface.viewers.IStructuredSelection

/** 
 * used in extension point 
 * {@link JUnitLaunchShortcut}.EXTENSION_POINT_LAUNCHER_ID
 */
interface Launcher {
	def boolean launch(IStructuredSelection selection, IProject project, String mode, boolean parameterize)
}
