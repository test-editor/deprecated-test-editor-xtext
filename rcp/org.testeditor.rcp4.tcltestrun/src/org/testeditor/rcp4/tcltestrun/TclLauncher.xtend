package org.testeditor.rcp4.tcltestrun

import java.util.Map
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.viewers.IStructuredSelection

interface TclLauncher {

	public def LaunchResult launchTest(IStructuredSelection selection, IProject project, String elementId,
		IProgressMonitor monitor, Map<String, Object> options)

}
