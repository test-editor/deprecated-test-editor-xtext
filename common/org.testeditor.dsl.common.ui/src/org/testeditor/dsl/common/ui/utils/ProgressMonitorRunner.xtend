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
 * itemis AG
 *******************************************************************************/
package org.testeditor.dsl.common.ui.utils

import org.eclipse.jface.dialogs.ProgressMonitorDialog
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.swt.widgets.Display
import org.slf4j.LoggerFactory

class ProgressMonitorRunner {

	static val logger = LoggerFactory.getLogger(ProgressMonitorRunner)

	def void run(IRunnableWithProgress runnableWithProgress) {
		try {
			new ProgressMonitorDialog(Display.current.activeShell).run(true, true, runnableWithProgress)
		} catch(InterruptedException e) {
			logger.debug("User aborted running operation.")
		}
	}
}
