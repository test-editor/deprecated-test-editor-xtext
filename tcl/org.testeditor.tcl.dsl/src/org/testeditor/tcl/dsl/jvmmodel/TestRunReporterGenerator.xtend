package org.testeditor.tcl.dsl.jvmmodel

import java.util.List
import javax.inject.Inject
import org.apache.commons.lang3.StringEscapeUtils
import org.apache.commons.lang3.StringUtils
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.slf4j.LoggerFactory
import org.testeditor.fixture.core.TestRunReporter.Action
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit
import org.testeditor.tcl.VariableReference

class TestRunReporterGenerator {
	
	@Inject TclExpressionBuilder expressionBuilder

	static val logger = LoggerFactory.getLogger(TestRunReporterGenerator)

	public def List<Object> buildReporterCall(JvmType type, SemanticUnit unit, Action action, String message, String id, String status,
		String reporterInstanceVariableName, List<VariableReference> variables, JvmTypeReference stringTypeReference) {
		val variablesValuesList = if (variables !== null){
			try {
			variables.filterNull.map[
				'''"«variable?.name»", «expressionBuilder.buildReadExpression(it, stringTypeReference)»''' //TODO: must yield variable access reference as defined in tcl! for json eg.
			].filterNull.join(', ')			
			}catch(RuntimeException e) {
				logger.warn('error during generation of parameter passing for reporting', e)
				''
			}
		} else { '' }
			
		val escapedMessage = StringEscapeUtils.escapeJava(message.trim)

		if (type === null) {
			logger.error('''typeRef='SemanticUnit' could not be resolved. This usually is a classpath problem (e.g. core-fixture unknown).''')
			return #['''
				// TODO: typeRef='SemanticUnit' could not be resolved.
				'''.toString]
		} else {
			return #['''
			
			«generateCommentPrefix»«initIdVar(action, id)»«reporterInstanceVariableName».«action.toString.toLowerCase»('''.toString, type, '''.«unit.name», "«escapedMessage»", «id», "«status»", variables(«variablesValuesList»));'''.toString.replaceAll('" *\\+ *"', '') ];
		}
	}
	
	private def String initIdVar(Action action, String idVar) {
		if(Action.ENTER.equals(action)){
			'''String «idVar»=getNewId(); '''
		}	
	}
	
	private def String generateCommentPrefix() {
		val prefix = System.getenv('TE_REPORTER_CALL_COMMENT_PREFIX_CHAR')
		val prefixCount = System.getenv('TE_REPORTER_CALL_COMMENT_PREFIX_COUNT')
		if ((prefix != null) && (prefixCount != null)) {
			try{
				return '''/*«StringUtils.repeat(prefix, Integer.parseInt(prefixCount))»*/'''				
			}catch(NumberFormatException nfe) {
			}
		}
		return ''
	}

}
