/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
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

import org.eclipse.xtext.conversion.IValueConverterService;
import org.eclipse.xtext.generator.IGenerator;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.testeditor.aml.dsl.conversion.AmlValueConverterService;
import org.testeditor.aml.dsl.generator.XmlGenerator;
import org.testeditor.aml.dsl.naming.AmlQualifiedNameProvider;
import org.testeditor.aml.dsl.scoping.AmlScopeProvider;

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
	
	@Override
	public Class<? extends IScopeProvider> bindIScopeProvider() {
		return AmlScopeProvider.class;
	}
	
}
