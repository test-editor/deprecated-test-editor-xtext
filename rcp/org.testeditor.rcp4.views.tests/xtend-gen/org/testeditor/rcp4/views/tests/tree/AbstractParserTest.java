/**
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
 */
package org.testeditor.rcp4.views.tests.tree;

import javax.inject.Inject;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.testeditor.aml.AmlModel;
import org.testeditor.aml.ModelUtil;
import org.testeditor.aml.dsl.tests.AbstractTest;

@SuppressWarnings("all")
public abstract class AbstractParserTest extends AbstractTest {
  @Inject
  protected ParseHelper<AmlModel> parser;
  
  @Inject
  @Extension
  protected ModelUtil modelUtil;
  
  protected AmlModel parse(final CharSequence input) {
    try {
      return this.parser.parse(input);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
