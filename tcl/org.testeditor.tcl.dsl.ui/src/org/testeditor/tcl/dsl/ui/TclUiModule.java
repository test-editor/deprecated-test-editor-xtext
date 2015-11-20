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
package org.testeditor.tcl.dsl.ui;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.xtext.ui.editor.contentassist.ITemplateProposalProvider;
import org.eclipse.xtext.ui.editor.hyperlinking.IHyperlinkHelper;
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfiguration;
import org.eclipse.xtext.ui.editor.syntaxcoloring.ISemanticHighlightingCalculator;
import org.testeditor.tcl.dsl.ui.contentassist.TclTemplateProposalProvider;
import org.testeditor.tcl.dsl.ui.highlighting.TclHighlightingConfiguration;
import org.testeditor.tcl.dsl.ui.highlighting.TclSemanticHighlightingCalculator;
import org.testeditor.tcl.dsl.ui.navigation.TclHyperLinkHelper;

public class TclUiModule extends org.testeditor.tcl.dsl.ui.AbstractTclUiModule {
    
    public TclUiModule(AbstractUIPlugin plugin) {
        super(plugin);
    }
    
    @Override
    public Class<? extends IHighlightingConfiguration> bindIHighlightingConfiguration() {
        return TclHighlightingConfiguration.class;
    }

    @Override
    public Class<? extends ISemanticHighlightingCalculator> bindISemanticHighlightingCalculator() {
        return TclSemanticHighlightingCalculator.class;
    }
   
    @Override
    public Class<? extends IHyperlinkHelper> bindIHyperlinkHelper() {
        return TclHyperLinkHelper.class;
    }
    
    @Override
    public Class<? extends ITemplateProposalProvider> bindITemplateProposalProvider() {
        return TclTemplateProposalProvider.class;
    }

}
