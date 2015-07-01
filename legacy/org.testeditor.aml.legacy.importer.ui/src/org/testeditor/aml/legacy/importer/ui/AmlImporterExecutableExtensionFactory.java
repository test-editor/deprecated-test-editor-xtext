package org.testeditor.aml.legacy.importer.ui;

import org.osgi.framework.Bundle;
import org.testeditor.aml.dsl.ui.AmlExecutableExtensionFactory;

public class AmlImporterExecutableExtensionFactory extends AmlExecutableExtensionFactory {

	@Override
	protected Bundle getBundle() {
		return Activator.getDefault().getBundle();
	}
	
}
