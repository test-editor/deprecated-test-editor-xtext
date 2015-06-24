package org.testeditor.aml.dsl;

import org.eclipse.xtext.generator.IGenerator;
import org.testeditor.aml.dsl.generator.OldXmlGenerator;

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
public class AmlRuntimeModule extends org.testeditor.aml.dsl.AbstractAmlRuntimeModule {

	/**
	 * Right now we don't generate Java classes but the old
	 * XML files. This will be changed in the future.
	 */
	@Override
	public Class<? extends IGenerator> bindIGenerator() {
		return OldXmlGenerator.class;
	}
	
}
