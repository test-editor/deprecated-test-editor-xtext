package org.testeditor.aml.dsl;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {

	static {
		NLS.initializeMessages("messages", Messages.class); //$NON-NLS-1$
	}
	
	public static String Validation_Component_Type_Missing;
	public static String Validation_Component_Cycle;

}
