package org.testeditor.dsl.common.ui.utils

import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.jface.dialogs.ProgressMonitorDialog
import org.eclipse.swt.widgets.Display
import java.lang.reflect.InvocationTargetException

class ProgressMonitorRunner {
	
	def run((IProgressMonitor)=>void runnable) {
			new ProgressMonitorDialog(Display.current.activeShell).run(false, false,
				new IRunnableWithProgress() {

					override run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
						runnable.apply(monitor)
					}

				})

	}
	

//	IProgressMonitor monitor
//
//	def void run(IRunnableWithProgress runnable) {
//		monitor = new ProgressMonitorDialog(Display.current.activeShell)
//		monitor.run(false, false, runnable)
//	}
//
//	def getMonitor() {
//		monitor
//	}

}
