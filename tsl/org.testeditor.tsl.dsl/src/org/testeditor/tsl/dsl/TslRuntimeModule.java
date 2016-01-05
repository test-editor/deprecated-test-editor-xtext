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
package org.testeditor.tsl.dsl;

import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.testeditor.tsl.dsl.naming.TslQualifiedNameProvider;

public class TslRuntimeModule extends org.testeditor.tsl.dsl.AbstractTslRuntimeModule {

    @Override
    public Class<? extends IQualifiedNameProvider> bindIQualifiedNameProvider() {
        return TslQualifiedNameProvider.class;
    }
    
}
