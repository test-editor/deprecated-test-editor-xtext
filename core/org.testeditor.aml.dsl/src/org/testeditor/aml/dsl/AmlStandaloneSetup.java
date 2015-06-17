package org.testeditor.aml.dsl;

/**
 * Initialization support for running Xtext languages 
 * without equinox extension registry
 */
public class AmlStandaloneSetup extends AmlStandaloneSetupGenerated{

	public static void doSetup() {
		new AmlStandaloneSetup().createInjectorAndDoEMFRegistration();
	}
}

