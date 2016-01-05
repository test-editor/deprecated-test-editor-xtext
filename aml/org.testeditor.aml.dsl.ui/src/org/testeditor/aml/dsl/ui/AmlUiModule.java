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
package org.testeditor.aml.dsl.ui;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfiguration;
import org.eclipse.xtext.ui.editor.syntaxcoloring.ISemanticHighlightingCalculator;
import org.eclipse.xtext.ui.wizard.IProjectCreator;
import org.testeditor.aml.dsl.ui.highlighting.AmlHighlightingConfiguration;
import org.testeditor.aml.dsl.ui.highlighting.AmlSemanticHighlightingCalculator;
import org.testeditor.aml.dsl.ui.wizard.CustomizedAmlProjectCreator;

/**
 * Use this class to register components to be used within the IDE.
 */
public class AmlUiModule extends org.testeditor.aml.dsl.ui.AbstractAmlUiModule {

	public AmlUiModule(AbstractUIPlugin plugin) {
		super(plugin);
	}

	@Override
	public Class<? extends IHighlightingConfiguration> bindIHighlightingConfiguration() {
		return AmlHighlightingConfiguration.class;
	}

	@Override
	public Class<? extends ISemanticHighlightingCalculator> bindISemanticHighlightingCalculator() {
		return AmlSemanticHighlightingCalculator.class;
	}
	
	@Override
	public Class<? extends IProjectCreator> bindIProjectCreator() {
		return CustomizedAmlProjectCreator.class;
	}
	
}
