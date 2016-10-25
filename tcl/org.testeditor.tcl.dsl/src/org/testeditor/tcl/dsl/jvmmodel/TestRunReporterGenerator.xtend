package org.testeditor.tcl.dsl.jvmmodel

import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit;
import org.apache.commons.lang3.StringEscapeUtils
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder

class TestRunReporterGenerator {
	
	public def void appendReporterEnterCall(ITreeAppendable output, JvmTypeReferenceBuilder typeReferenceBuilder, SemanticUnit unit,
		String message, String reporterInstanceVariableName) {
		val escapedMessage=StringEscapeUtils.escapeJava(message.trim)
		// all above STEP will add a newLine 
		if (unit.ordinal < SemanticUnit.STEP.ordinal) {
			output.newLine
		}
		if (typeReferenceBuilder == null) { // this happens in some testcases, in which this output is 
			output.append('''// TODO: «reporterInstanceVariableName».enter(«unit.name», "«escapedMessage»");''').newLine
		} else {
			val typeRef = typeReferenceBuilder.typeRef(SemanticUnit)
			output.append('''«reporterInstanceVariableName».enter(''').append(typeRef.type).append('''.«unit.name», "«escapedMessage»");''').
				newLine
		}
	}

}