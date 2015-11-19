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
package org.testeditor.demo.swing

# GreetingTest implements GreetingSpec
 
* Start the famous greetings application.
 
    Mask: GreetingApplication
    - Starte Anwendung "org.testeditor.demo.swing.GreetingApplication"
 
* Send greetings "Hello World" to the world.
 
    Mask: GreetingApplication
    - Gebe "Hello World" in das Feld <Input> ein 
    - Klicke auf <GreetButton>
    - Warte "2000" ms   
 
* Stop the famous greeting application.

	Mask: GreetingApplication
	- Stoppe Anwendung