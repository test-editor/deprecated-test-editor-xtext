package org.testeditor.aml.dsl.ui;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfiguration;
import org.eclipse.xtext.ui.editor.syntaxcoloring.ISemanticHighlightingCalculator;
import org.testeditor.aml.dsl.ui.highlighting.AmlSemanticHighlightingCalculator;
import org.testeditor.aml.dsl.ui.highlighting.AmlHighlightingConfiguration;

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
	
}
