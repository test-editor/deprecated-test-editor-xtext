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
package org.testeditor.demo.swing;

import org.junit.Test;
import org.testeditor.fixture.swing.SwingFixture;

public class HandWrittenTest {

    private SwingFixture fixture = new SwingFixture();
    
    @Test
    public void demo() {
        fixture.startApplication(GreetingApplication.class.getName());
        fixture.stopApplication();
    }
    
    
}
