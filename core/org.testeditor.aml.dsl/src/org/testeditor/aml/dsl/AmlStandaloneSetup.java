package org.testeditor.aml.dsl;

import org.eclipse.emf.ecore.EPackage;
import org.testeditor.aml.model.ModelPackage;

import com.google.inject.Injector;

public class AmlStandaloneSetup extends AmlStandaloneSetupGenerated {

	public static void doSetup() {
		new AmlStandaloneSetup().createInjectorAndDoEMFRegistration();
	}

	@Override
	public Injector createInjectorAndDoEMFRegistration() {
		EPackage.Registry.INSTANCE.put(ModelPackage.eINSTANCE.getNsURI(), ModelPackage.eINSTANCE);
		return super.createInjectorAndDoEMFRegistration();
	}

}
