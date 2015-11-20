package org.testeditor.tcl.dsl.ui.contentassist

import javax.inject.Inject
import org.eclipse.jface.text.templates.ContextTypeRegistry
import org.eclipse.jface.text.templates.Template
import org.eclipse.jface.text.templates.TemplateContext
import org.eclipse.jface.text.templates.persistence.TemplateStore
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ITemplateAcceptor
import org.eclipse.xtext.ui.editor.templates.ContextTypeIdHelper
import org.eclipse.xtext.ui.editor.templates.DefaultTemplateProposalProvider
import org.testeditor.aml.model.Component
import org.testeditor.aml.model.ModelUtil
import org.testeditor.aml.model.TemplateText
import org.testeditor.aml.model.TemplateVariable
import org.testeditor.tcl.TestStep
import org.testeditor.aml.model.InteractionType

class TclTemplateProposalProvider extends DefaultTemplateProposalProvider {

	@Inject extension ModelUtil
	@Inject IQualifiedNameProvider nameProvider

	@Inject
	new(TemplateStore templateStore, ContextTypeRegistry registry, ContextTypeIdHelper helper) {
		super(templateStore, registry, helper)
	}

	/**
	 * Customize template creation to support dynamic templates.
	 */
	override protected createTemplates(TemplateContext templateContext, ContentAssistContext context,
		ITemplateAcceptor acceptor) {
		super.createTemplates(templateContext, context, acceptor)

		val model = context.currentModel
		if (model instanceof TestStep) {
			val component = model.context.component
			component?.proposeAvailableInteractions(model, templateContext, context, acceptor)
		}
	}

	/**
	 * Provide dynamic templates for all available interaction types.
	 */
	def void proposeAvailableInteractions(Component component, TestStep step, TemplateContext templateContext,
		ContentAssistContext context, ITemplateAcceptor acceptor) {
		val interactionTypes = component.allInteractionTypes
		interactionTypes.forEach [
			val template = createTemplate
			val relevance = 500
			val proposal = createProposal(template, templateContext, context, getImage(template), relevance)
			acceptor.accept(proposal)
		]
	}

	/**
	 * Create a dynamic template for {@link InteractionType}.
	 */
	private def Template createTemplate(InteractionType interactionType) {
		val proposalDescription = interactionType.template.restoreString(false)
		val proposalTemplate = '- ' + interactionType.template.restoreString(true)
		val templateId = nameProvider.getFullyQualifiedName(interactionType).toString
		return new Template(proposalDescription, 'test step', templateId, proposalTemplate, false)
	}

	private def String restoreString(org.testeditor.aml.model.Template template, boolean asTemplate) {
		return template.contents.map [
			switch (it) {
				TemplateVariable case name == ModelUtil.TEMPLATE_VARIABLE_ELEMENT:
					if (asTemplate) '''<${«name»}>''' else '''<«name»>'''
				TemplateVariable:
					if (asTemplate) '''"${«name»}"''' else '''"«name»"'''
				TemplateText:
					value
			}
		].join(' ')
	}

}