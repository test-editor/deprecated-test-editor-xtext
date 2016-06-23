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
package org.testeditor.tml.dsl

import com.google.inject.Binder
import com.google.inject.name.Names
import org.eclipse.xtext.scoping.IScopeProvider
import org.eclipse.xtext.scoping.impl.AbstractDeclarativeScopeProvider
import org.testeditor.tml.dsl.conversion.TmlValueConverterService
import org.testeditor.tml.dsl.naming.TmlQualifiedNameProvider
import org.testeditor.tml.dsl.scoping.TmlDelegateScopeProvider

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
class TmlRuntimeModule extends AbstractTmlRuntimeModule {

	override bindIQualifiedNameProvider() {
		return TmlQualifiedNameProvider
	}

	override bindIValueConverterService() {
		return TmlValueConverterService
	}

	override configureIScopeProviderDelegate(Binder binder) {
		binder.bind(IScopeProvider).annotatedWith(Names.named(AbstractDeclarativeScopeProvider.NAMED_DELEGATE)).to(
			TmlDelegateScopeProvider)
	}
}
