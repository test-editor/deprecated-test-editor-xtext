package org.testeditor.tcl.dsl.jvmmodel

import java.util.List
import javax.inject.Inject
import org.apache.commons.lang3.StringEscapeUtils
import org.eclipse.xtext.common.types.JvmType
import org.slf4j.LoggerFactory
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit
import org.testeditor.tcl.VariableReference

class TestRunReporterGenerator {
	
	@Inject TclExpressionBuilder expressionBuilder

	static val logger = LoggerFactory.getLogger(TestRunReporterGenerator)

	public def List<Object> buildReporterEnterCall(JvmType type, SemanticUnit unit, String message,
		String reporterInstanceVariableName, List<VariableReference> variables) {
		val unescapedSplicingPostfix = variableSplicingText(variables)
			
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
			«reporterInstanceVariableName».enter('''.toString, type, '''.«unit.name», "«escapedMessage»«unescapedSplicingPostfix?:''»");
			'''.toString.replaceAll('" *\\+ *"', '') ];
		}
	}
	
	/**
	 * Get a string suffix that will close a message string and append code to be
	 * added to the generated reporting line, writing out the variables and their
	 * content during runtime (that is during execution of the test).
	 * 
	 * @see org.testeditor.tcl.dsl.jvmmodel.TestRunReporterGeneratorTest#testUnsplicingOfVariablePostfixString() Unit-Test
	 */
	private def String variableSplicingText(List<VariableReference> variables) {
		if (variables !== null && !variables.empty) {
			val splicableStrings = variables.map [
				'''"«variable.name» = '" + «expressionBuilder.variableToVarName(variable)» + "'«''»'''
			].join('''" + ", " + ''')
			return '''" + " // " + «splicableStrings»'''
		} else {
			return ''
		}
	}

	private def boolean shouldPrintNewLine(SemanticUnit unit) {
		return unit != SemanticUnit.STEP
	}

}
