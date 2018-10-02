package org.testeditor.tcl.dsl.jvmmodel

import java.util.List
import java.util.Optional
import javax.inject.Inject
import org.apache.commons.lang3.StringEscapeUtils
import org.apache.commons.lang3.StringUtils
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.slf4j.LoggerFactory
import org.testeditor.fixture.core.MaskingString
import org.testeditor.fixture.core.TestRunReporter.Action
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit
import org.testeditor.fixture.core.TestRunReporter.Status
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.util.TclModelUtil

class TestRunReporterGenerator {

	@Inject TclExpressionBuilder expressionBuilder
	@Inject TclGeneratorConfig generatorConfig
	@Inject TclExpressionTypeComputer typeComputer
	@Inject extension TclModelUtil

	static val logger = LoggerFactory.getLogger(TestRunReporterGenerator)

	public def List<Object> buildReporterCall(JvmType type, SemanticUnit unit, Action action, String message, String id, Status status,
		String reporterInstanceVariableName, List<VariableReference> variables, JvmTypeReference stringTypeReference) {
		val variablesValuesList = if (variables !== null) {
				try {
					variables.filterNull.map [
						val varType = typeComputer.determineType(variable, Optional.empty)?.qualifiedName
						val maskingType = MaskingString.name
						
						'''"«if (it instanceof VariableReferencePathAccess) { 
							StringEscapeUtils.escapeJava(restoreString)
						}else{ 
							variable.name
						}»", «if (maskingType.equals(varType)) {
							'"*****"'
						} else {
							expressionBuilder.buildReadExpression(it, stringTypeReference)
						}»'''
					].filterNull.join(', ')
				} catch (RuntimeException e) {
					logger.warn('error during generation of parameter passing for reporting', e)
					''
				}
			} else {
				''
			}

		val escapedMessage = StringEscapeUtils.escapeJava(message.trim)

		if (type === null) {
			logger.error('''typeRef='SemanticUnit' could not be resolved. This usually is a classpath problem (e.g. core-fixture unknown).''')
			return #['''
				// TODO: typeRef='SemanticUnit' could not be resolved.
			'''.toString]
		} else {
			return #['''
			
			«generateCommentPrefix»«initIdVar(action, id)»«reporterInstanceVariableName».«action.toString.toLowerCase»('''.toString, type,
				'''.«unit.name», "«escapedMessage»", «id», TestRunReporter.Status.«status.name», variables(«variablesValuesList»));'''.toString.replaceAll('" *\\+ *"',
					'')];
		}
	}

	private def String initIdVar(Action action, String idVar) {
		if (Action.ENTER.equals(action)) {
			'''String «idVar»=newVarId(); '''
		}
	}

	private def String generateCommentPrefix() {
		val prefix = generatorConfig.reporterCallCommentPrefixChar
		if (prefix != null) {
			return '''/*«StringUtils.repeat(prefix, generatorConfig.reporterCallCommentPrefixCount)»*/ '''
		} else {
			return ''
		}
	}

}
