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
package org.testeditor.dsl.common.util

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.PrintStream
import org.slf4j.LoggerFactory

class OutputStreamCopyUtil extends Thread {

	static val logger = LoggerFactory.getLogger(OutputStreamCopyUtil);

	BufferedReader reader
	PrintStream out

	new(InputStream source, PrintStream out) {
		this.reader = new BufferedReader(new InputStreamReader(source))
		this.out = out
	}

	override void run() {
		try {
			var String message = ""
			while (message !== null) {
				out.println(message)
				logger.trace(message)
				message = reader.readLine()
			}
		} catch (IOException e) {
			logger.error("Cannot connect to process ouput stream", e)
		}
	}

}
