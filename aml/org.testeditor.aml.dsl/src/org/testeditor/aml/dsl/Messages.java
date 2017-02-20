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
package org.testeditor.aml.dsl;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {

	static {
		NLS.initializeMessages("messages", Messages.class); //$NON-NLS-1$
	}
	
	public static String Validation_Component_Type_Missing;
	public static String Validation_Component_Cycle;
	public static String Validation_VariableReference_MissingName;
	public static String Validation_ValueSpaceAssignment_NonUnique;
	public static String Validation_MethodReference_InvalidParameterList;
	public static String Validation_RegExValueSpace_InvalidRegEx;
	public static String Validation_TemplateCode_NotUnique;
	public static String Validation_InteractionType_Name_Dublicate;

}
