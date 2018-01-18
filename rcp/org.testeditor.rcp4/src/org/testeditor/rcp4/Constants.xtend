/** 
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 */
package org.testeditor.rcp4

/** 
 * Constants for common usage in different classes of the RCP 4 bundle.
 */
interface Constants {
	String CONFIGURATION_STORE = 'TE_RCP4_CONFIGURATION_STORE'
	String RESET_APP_PROPERTY = 'TE_RESET_APPLICATION_UI'
	String TE_WS_VERSION_ID = 'TE_WS:VERSION_ID'
	String TE_WS_VERSION = '2.0.2'
	
	/** as defined in the Application.e4xmi */
	String MAIN_PERSPECTIVE_STACK_ID = 'org.testeditor.rcp4.perspectivestack'  
	/** as defined in the Application.e4xmi */
	String PERSPECTIVE_ID = 'org.testeditor.rcp4.perspective' 
}
