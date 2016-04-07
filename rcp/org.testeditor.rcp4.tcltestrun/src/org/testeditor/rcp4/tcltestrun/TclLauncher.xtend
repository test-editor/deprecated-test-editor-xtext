package org.testeditor.rcp4.tcltestrun

import org.eclipse.core.resources.IProject
import java.util.Map
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.viewers.IStructuredSelection

interface TclLauncher {
	
	public static val String RETURN_CODE = "returnCode"
	public static val String EXPECTED_FILE = "expectedFile"
	public static val String EXCEPTION = "exception"

	public def Map<String, Object> launchTest(IStructuredSelection selection, IProject project, String elementId,
		IProgressMonitor monitor, Map<String,Object> options)
}