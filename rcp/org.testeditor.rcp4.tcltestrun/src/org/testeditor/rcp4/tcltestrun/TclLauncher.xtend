package org.testeditor.rcp4.tcltestrun

import java.util.Map
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import java.util.List

interface TclLauncher {

	public def LaunchResult launchTest(List<String> testCasesList, IProject project, 
		IProgressMonitor monitor, Map<String, Object> options)

}
