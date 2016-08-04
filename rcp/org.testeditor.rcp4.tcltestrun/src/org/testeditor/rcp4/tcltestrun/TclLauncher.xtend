package org.testeditor.rcp4.tcltestrun

import java.util.List
import java.util.Map
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor

interface TclLauncher {

	public def LaunchResult launchTest(List<String> testCases, IProject project, 
		IProgressMonitor monitor, Map<String, Object> options)

}
