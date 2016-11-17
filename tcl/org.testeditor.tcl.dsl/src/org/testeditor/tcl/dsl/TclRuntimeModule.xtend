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
package org.testeditor.tcl.dsl

import com.google.inject.Binder
import com.google.inject.name.Names
import org.eclipse.xtext.parser.antlr.ISyntaxErrorMessageProvider
import org.eclipse.xtext.scoping.IScopeProvider
import org.eclipse.xtext.scoping.impl.AbstractDeclarativeScopeProvider
import org.testeditor.tcl.dsl.conversion.TclValueConverterService
import org.testeditor.tcl.dsl.messages.TclSyntaxErrorMessageProvider
import org.testeditor.tcl.dsl.naming.TclQualifiedNameProvider
import org.testeditor.tcl.dsl.scoping.TclDelegateScopeProvider
import org.testeditor.tcl.dsl.validation.FileExtensionValidator
import org.testeditor.tcl.dsl.validation.AdjustedXbaseConfigurableIssueCodes

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
class TclRuntimeModule extends AbstractTclRuntimeModule {

	override bindConfigurableIssueCodesProvider() {
		return AdjustedXbaseConfigurableIssueCodes
	}

	override bindIQualifiedNameProvider() {
		return TclQualifiedNameProvider
	}

	def Class<? extends ISyntaxErrorMessageProvider> bindISyntaxErrorMessageProvider() {
		return TclSyntaxErrorMessageProvider
	}

	override bindIValueConverterService() {
		return TclValueConverterService
	}

	override configureIScopeProviderDelegate(Binder binder) {
		binder.bind(IScopeProvider).annotatedWith(Names.named(AbstractDeclarativeScopeProvider.NAMED_DELEGATE)).to(
			TclDelegateScopeProvider)
	}

	def void configureAdditionalValidators(Binder binder) {
		binder.bind(FileExtensionValidator).asEagerSingleton
	}

}
