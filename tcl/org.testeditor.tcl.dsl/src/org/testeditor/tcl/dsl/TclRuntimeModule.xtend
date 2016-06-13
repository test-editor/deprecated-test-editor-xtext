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

import org.testeditor.tcl.dsl.naming.TclQualifiedNameProvider
import org.testeditor.tcl.dsl.messages.TclSyntaxErrorMessageProvider
import org.eclipse.xtext.parser.antlr.ISyntaxErrorMessageProvider

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
class TclRuntimeModule extends AbstractTclRuntimeModule {

	override bindIQualifiedNameProvider() {
		return TclQualifiedNameProvider
	}

    def Class<? extends ISyntaxErrorMessageProvider> bindISyntaxErrorMessageProvider() {
        return TclSyntaxErrorMessageProvider
    }
}
