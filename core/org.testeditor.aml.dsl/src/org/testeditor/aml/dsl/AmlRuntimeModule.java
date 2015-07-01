package org.testeditor.aml.dsl;

import org.eclipse.xtext.conversion.IValueConverterService;
import org.eclipse.xtext.generator.IGenerator;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.testeditor.aml.dsl.conversion.AmlValueConverterService;
import org.testeditor.aml.dsl.generator.XmlGenerator;
import org.testeditor.aml.dsl.naming.AmlQualifiedNameProvider;

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
		return XmlGenerator.class;
	}
	
	@Override
	public Class<? extends IValueConverterService> bindIValueConverterService() {
		return AmlValueConverterService.class;
	}
	
	@Override
	public Class<? extends IQualifiedNameProvider> bindIQualifiedNameProvider() {
		return AmlQualifiedNameProvider.class;
	}
	
}
