package org.testeditor.tcl.dsl.jvmmodel

import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit;
import org.apache.commons.lang3.StringEscapeUtils
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder
import org.slf4j.LoggerFactory

class TestRunReporterGenerator {

	static val logger = LoggerFactory.getLogger(TestRunReporterGenerator)

	public def void appendReporterEnterCall(ITreeAppendable output, JvmTypeReferenceBuilder typeReferenceBuilder,
		SemanticUnit unit, String message, String reporterInstanceVariableName) {
		val escapedMessage = StringEscapeUtils.escapeJava(message.trim)

		if (unit.shouldPrintNewLine) {
			output.newLine
		}
		val type = typeReferenceBuilder?.typeRef(SemanticUnit)?.type
		if (type === null) {
			output.append('''// TODO: typeRef='SemanticUnit' could not be resolved.''').newLine
			logger.error('''typeRef='SemanticUnit' could not be resolved. This usually is a classpath problem (e.g. core-fixture unknown).''')
		} else {
			output.append('''«reporterInstanceVariableName».enter(''').append(type).
				append('''.«unit.name», "«escapedMessage»");''').newLine
		}
	}

	private def boolean shouldPrintNewLine(SemanticUnit unit) {
		return unit != SemanticUnit.STEP
	}

}
