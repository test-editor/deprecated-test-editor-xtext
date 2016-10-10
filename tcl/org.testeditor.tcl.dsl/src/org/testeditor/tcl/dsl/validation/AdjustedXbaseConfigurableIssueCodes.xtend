package org.testeditor.tcl.dsl.validation

import org.eclipse.xtext.preferences.PreferenceKey
import org.eclipse.xtext.util.IAcceptor
import org.eclipse.xtext.validation.SeverityConverter
import org.eclipse.xtext.xbase.validation.IssueCodes
import org.eclipse.xtext.xbase.validation.XbaseConfigurableIssueCodes

/** adjust defaults of XbaseConfigurableIssueCodes */
class AdjustedXbaseConfigurableIssueCodes extends XbaseConfigurableIssueCodes {

	override initialize(IAcceptor<PreferenceKey> iAcceptor) {
		super.initialize(iAcceptor)
		// override default (which ignores java problems in derived java files) 
		iAcceptor.accept(create(IssueCodes.COPY_JAVA_PROBLEMS, SeverityConverter.SEVERITY_ERROR))
	}

}
