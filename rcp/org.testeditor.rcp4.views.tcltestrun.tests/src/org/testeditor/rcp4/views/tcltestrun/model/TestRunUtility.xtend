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
package org.testeditor.rcp4.views.tcltestrun.model

class TestRunUtility {

	def static public String getTestResult() {
		'''
		<?xml version="1.0" encoding="UTF-8" standalone="no"?>
		<testrun errors="0" failures="0" ignored="0" name="java" project="org.testeditor.rcp4.uatests" started="1" tests="1">
		</testrun>
		'''
	}
		
}