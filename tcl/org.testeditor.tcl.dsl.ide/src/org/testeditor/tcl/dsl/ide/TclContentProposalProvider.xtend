package org.testeditor.tcl.dsl.ide

import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ide.editor.contentassist.IIdeContentProposalAcceptor
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalProvider
import org.slf4j.LoggerFactory
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateContainer
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.dsl.services.TclGrammarAccess
import org.testeditor.tcl.util.TclModelUtil

class TclContentProposalProvider extends IdeContentProposalProvider {

	static val logger = LoggerFactory.getLogger(TclContentProposalProvider)

	@Inject JvmTypeReferenceUtil typeUtil
	@Inject SimpleTypeComputer typeComputer
	@Inject extension TclGrammarAccess grammarAccess
	@Inject extension TclModelUtil
	@Inject extension ModelUtil

	override protected _createProposals(RuleCall ruleCall, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		if (ruleCall.rule === grammarAccess.testStepRule) {
			logger.info('test step rule selected')
			val entry = proposalCreator.createProposal('- ', context)
			val prio = proposalPriorities.getDefaultPriority(entry)
			acceptor.accept(entry, prio)
		} else if (ruleCall.rule === grammarAccess.stepContentElementRule) {
			logger.info('step content element rule selected')
			val model = context.currentModel

			if (model instanceof TestStep) {
				model.envParams.forEach [
					acceptor.accept(proposalCreator.createProposal("@" + name, context), 20)
				]
				model.enclosingMacroParameters.forEach [
					acceptor.accept(proposalCreator.createProposal("@" + name, context), 20)
				]
				val interaction = model.interaction
				val component = model.componentContext?.component
				if (component !== null) {
					val possibleElements = component.elements.filter [
						val interactionTypes = componentElementInteractionTypes
						return interactionTypes.contains(interaction)
					]
					// need to consider whether the completion should contain the '>' as well
					val currentNode = context.currentNode
					val includeClosingBracket = !currentNode.text.contains('>') && (currentNode.nextSibling === null || !currentNode.nextSibling.text.contains('>'))
					possibleElements.forEach [
						val proposal = '''<«name»«IF includeClosingBracket»>«ENDIF»'''
						acceptor.accept(proposalCreator.createProposal(proposal, context), 10)
					]

					val interactions = component.allInteractionTypes
					interactions.forEach [
						val element = template.restoreString
						val proposal = proposalCreator.createProposal(element, context)
						acceptor.accept(proposal, 600)
					]

				}
			}

		}
		super._createProposals(ruleCall, context, acceptor)
	}

	private def String restoreString(Template template) {
		return template.contents.map [
			switch (it) {
				TemplateVariable case name == ModelUtil.TEMPLATE_VARIABLE_ELEMENT: '''<«name»>'''
				TemplateVariable:
					typedProposalString(EcoreUtil2.getContainerOfType(template, TemplateContainer))
				TemplateText:
					value
			}
		].join(' ')
	}

	private def String typedProposalString(TemplateVariable templateVariable, TemplateContainer templateContainer) {
		val expectedType = typeComputer.getExpectedType(templateVariable, templateContainer)
		if (expectedType.present) {
			typeUtil.initWith(templateVariable.eResource)
			if (typeUtil.isANumber(expectedType.get)) {
				return "1"
			} else if (typeUtil.isEnum(expectedType.get)) {
				return typeUtil.getEnumValues(expectedType.get).head
			} else if (typeUtil.isBoolean(expectedType.get)) {
				return "true"
			}
		}

		// default
		return '''"«templateVariable.name»"'''
	}

}
