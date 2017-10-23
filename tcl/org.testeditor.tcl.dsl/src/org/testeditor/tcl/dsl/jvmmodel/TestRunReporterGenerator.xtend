package org.testeditor.tcl.dsl.jvmmodel

import java.util.List
import org.apache.commons.lang3.StringEscapeUtils
import org.eclipse.xtext.common.types.JvmType
import org.slf4j.LoggerFactory
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit

class TestRunReporterGenerator {

	static val logger = LoggerFactory.getLogger(TestRunReporterGenerator)

	public def List<Object> buildReporterEnterCall(JvmType type, SemanticUnit unit, String message,
		String reporterInstanceVariableName) {
		val escapedMessage = StringEscapeUtils.escapeJava(message.trim)

		if (type === null) {
			logger.error('''typeRef='SemanticUnit' could not be resolved. This usually is a classpath problem (e.g. core-fixture unknown).''')
			return #['''
				// TODO: typeRef='SemanticUnit' could not be resolved.
				'''.toString]
		} else {
			return #['''
			«IF unit.shouldPrintNewLine»

			«ENDIF»
			«reporterInstanceVariableName».enter('''.toString, type, '''.«unit.name», "«escapedMessage»");
			'''.toString ];
		}
	}

	private def boolean shouldPrintNewLine(SemanticUnit unit) {
		return unit != SemanticUnit.STEP
	}

}
