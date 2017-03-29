package org.testeditor.aml.dsl.ui.contentassist

import com.google.inject.Inject
import org.eclipse.jface.text.templates.ContextTypeRegistry
import org.eclipse.jface.text.templates.Template
import org.eclipse.jface.text.templates.TemplateContext
import org.eclipse.jface.text.templates.persistence.TemplateStore
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ITemplateAcceptor
import org.eclipse.xtext.ui.editor.templates.ContextTypeIdHelper
import org.eclipse.xtext.ui.editor.templates.DefaultTemplateProposalProvider
import org.testeditor.aml.MethodReference
import org.testeditor.aml.dsl.services.AmlGrammarAccess

class AmlTemplateProposalProvider extends DefaultTemplateProposalProvider {

	static val DEFAULT_RELEVANCE = 1000

	String leftParenthesisContextTypeId
	String rightParenthesisContextTypeId

	@Inject
	new(TemplateStore templateStore, ContextTypeRegistry registry, ContextTypeIdHelper helper, AmlGrammarAccess grammarAccess) {
		super(templateStore, registry, helper)

		// Retrieve the contextTypeId for keywords '(' and ')' 
		val leftParenthesisKeyword = grammarAccess.methodReferenceAccess.leftParenthesisKeyword_3_0
		val rightParenthesisKeyword = grammarAccess.methodReferenceAccess.rightParenthesisKeyword_3_2
		leftParenthesisContextTypeId = helper.getId(leftParenthesisKeyword)
		rightParenthesisContextTypeId = helper.getId(rightParenthesisKeyword)
	}

	/**
	 * Customize template creation to support dynamic templates.
	 */
	override protected createTemplates(TemplateContext templateContext, ContentAssistContext context, ITemplateAcceptor acceptor) {
		super.createTemplates(templateContext, context, acceptor)

		if (!acceptor.canAcceptMoreTemplates || !context.prefix.nullOrEmpty) {
			return
		}

		// dispatch to responsible method
		val model = context.currentModel
		if (model instanceof MethodReference) {
			val template = createTemplate(model, templateContext, context)
			if (template !== null) {
				val proposal = doCreateProposal(template, templateContext, context, getImage(template), DEFAULT_RELEVANCE)
				acceptor.accept(proposal)
			}
		}
	}

	/**
	 * Creates templates for {@link MethodReference} if applicable.
	 * 
	 * Note that it's very difficult to add templates including the method name, i.e. if a reference to {@link MyFixture}
	 * is already given and we want to add a template such as {@code myMethod(param1, param2)}. The template
	 * context would be a cross-reference which is not supported by default. So we would need to register our own template
	 * context types which is some effort.
	 * 
	 * Hence, we only provide templates for parameters if the method has already been chosen.
	 */
	private def Template createTemplate(MethodReference model, TemplateContext templateContext, ContentAssistContext context) {
		// Check the context - we only want to add templates if the context is the parenthesis of the parameter list
		val contextTypeId = templateContext.contextType.id
		val contextIsParenthesis = contextTypeId == leftParenthesisContextTypeId || contextTypeId == rightParenthesisContextTypeId
		if (contextIsParenthesis) {
			if (model.operation !== null && !model.operation.eIsProxy && model.parameters.empty) {
				// there is already an operation configured, we only need to add parameters
				// TODO note that we might have multiple methods with the same name, but a different set of parameters
				return createParameterTemplate(model.operation, context.currentNode, contextTypeId)
			} // else: nothing we can do here
		}

		return null
	}

	/**
	 * Creates a template including all parameters for a method.
	 */
	private def Template createParameterTemplate(JvmOperation operation, INode currentNode, String contextTypeId) {
		// determine whether or not to include the parenthesis
		val previousText = currentNode.previousSibling?.text
		val nextText = currentNode.nextSibling?.text
		val includeLeftParenthesis = previousText.nullOrEmpty || !previousText.endsWith('(')
		val includeRightParenthesis = currentNode.text != ')' && nextText != ')'

		// create the actual template
		val parameterNames = operation.parameters.map[simpleName].map [
			// replace "locator" parameter name by "element"
			if (it == "locator") {
				return "element"
			}
			return it
		]
		val parameterList = parameterNames.join(', ', ['''${«it»}''']) // join parameters and surround by ${ }
		val pattern = '''«IF includeLeftParenthesis»(«ENDIF»«parameterList»«IF includeRightParenthesis»)«ENDIF»'''
		val name = pattern.replaceAll('\\$\\{|\\}', '') // remove ${ } from the name
		val description = '''parameter«IF parameterNames.size > 1»s«ENDIF» for «operation.simpleName»'''
		return new Template(name, description, contextTypeId, pattern, false)
	}

}
