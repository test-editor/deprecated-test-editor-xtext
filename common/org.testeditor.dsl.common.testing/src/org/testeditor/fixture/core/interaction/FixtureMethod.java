/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
package org.testeditor.fixture.core.interaction;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

// TODO temporary copy of https://github.com/test-editor/core-fixture/blob/feature/TE-241_restructure_fixtures/core/org.testeditor.fixture.core/src/main/java/org/testeditor/fixture/core/interaction/FixtureMethod.java

/**
 * Marks a method as being a fixture method, i.e. it can be called from the outside in order to interact with the 
 * application under test.
 * <p>
 * The method is not marked as @Inherited since a subclass may want to hide inherited fixture methods from the user. 
 * In this case it could simply override the inherited method and <strong>not</strong> annotate it. In this case, 
 * the tooling should not display the method.
 * </p>
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface FixtureMethod {
}