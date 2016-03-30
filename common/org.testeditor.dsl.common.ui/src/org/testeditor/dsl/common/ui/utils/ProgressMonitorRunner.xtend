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

import java.lang.reflect.InvocationTargetException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.dialogs.ProgressMonitorDialog
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.swt.widgets.Display

class ProgressMonitorRunner {
	
	def void run((IProgressMonitor)=>void runnable) {
			new ProgressMonitorDialog(Display.current.activeShell).run(false, false,
				new IRunnableWithProgress() {

					override run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
						runnable.apply(monitor)
					}

				})

	}
	
}
