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
package org.testeditor.aml.dsl

import com.google.inject.Binder
import org.eclipse.xtext.scoping.IScopeProvider
import org.eclipse.xtext.xbase.scoping.batch.IBatchScopeProvider
import org.testeditor.aml.dsl.conversion.AmlValueConverterService
import org.testeditor.aml.dsl.generator.XmlGenerator
import org.testeditor.aml.dsl.naming.AmlQualifiedNameProvider
import org.testeditor.aml.dsl.scoping.AmlDelegateScopeProvider
import org.testeditor.aml.dsl.scoping.AmlScopeProvider

import static com.google.inject.name.Names.named
import static org.eclipse.xtext.scoping.impl.AbstractDeclarativeScopeProvider.NAMED_DELEGATE

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
class AmlRuntimeModule extends AbstractAmlRuntimeModule {

	/**
	 * Right now we don't generate Java classes but the old
	 * XML files. This will be changed in the future.
	 */
	override bindIGenerator() {
		return XmlGenerator
	}

	override bindIValueConverterService() {
		return AmlValueConverterService
	}

	override bindIQualifiedNameProvider() {
		return AmlQualifiedNameProvider
	}

	// TODO this was override before?
	def Class<? extends IBatchScopeProvider> bindIBatchScopeProvider() {
		return AmlScopeProvider
	}

	override configureIScopeProviderDelegate(Binder binder) {
		binder.bind(IScopeProvider).annotatedWith(named(NAMED_DELEGATE)).to(AmlDelegateScopeProvider)
	}
}
