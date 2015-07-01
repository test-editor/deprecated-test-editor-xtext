package org.testeditor.aml.dsl.ui.contentassist

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.text.contentassist.CompletionProposal
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor
import org.eclipse.xtext.ui.resource.ProjectByResourceProvider
import org.testeditor.aml.dsl.services.AmlGrammarAccess
import org.eclipse.xtext.AbstractRule

class AmlProposalProvider extends AbstractAmlProposalProvider {

	@Inject
	ProjectByResourceProvider projectByResourceProvider

	@Inject
	AmlGrammarAccess grammarAccess

	/**
	 * Provides the project's name as default proposal, "com.example" as fall-back.
	 */
	override completeAmlModel_Package(EObject model, Assignment assignment, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		super.completeAmlModel_Package(model, assignment, context, acceptor)
		val project = projectByResourceProvider.getProjectContext(context.resource)
		acceptor.accept(createCompletionProposal(project?.name ?: "com.example", context))
	}

	/**
	 * Provide "AllActionGroups.xml" as a default for the file name of AllActionGroups.
	 */
	override completeAmlModel_FileNameAllActionGroups(EObject model, Assignment assignment, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		super.completeAmlModel_FileNameAllActionGroups(model, assignment, context, acceptor)
		acceptor.accept(createCompletionProposal('''"AllActionGroups.xml"''', context))
	}

	/**
	 * Provide "TechnicalBindingTypeCollection.xml" as a default for the file name of TechnicalBindingDefinition.
	 */
	override completeAmlModel_FileNameTechnicalBindings(EObject model, Assignment assignment, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		super.completeAmlModel_FileNameTechnicalBindings(model, assignment, context, acceptor)
		acceptor.accept(createCompletionProposal('''"TechnicalBindingTypeCollection.xml"''', context))
	}

	override complete_TemplateText(EObject model, RuleCall ruleCall, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		val node = context.currentNode
		if (node.grammarElement.isRuleCallTo(grammarAccess.STRINGRule)) {
			acceptor.accept(new CompletionProposal(''' ${}''', node.offset + node.length, 0, 3))
		} else {
			if (node.grammarElement == grammarAccess.WSRule) {
				if (!context.lastCompleteNode.grammarElement.isRuleCallTo(grammarAccess.STRINGRule)) {
					acceptor.accept(new CompletionProposal('''""''', context.offset, 0, 1))
				} // else: don't propose "" since we just had this
			}
		}
	}

	override complete_TemplateVariable(EObject model, RuleCall ruleCall, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		val node = context.currentNode
		if (node.grammarElement.isRuleCallTo(grammarAccess.TEMPLATE_VARIABLE_DEFRule)) {
			acceptor.accept(new CompletionProposal(''' ""''', node.offset + node.length, 0, 2))
		} else {
			if (node.grammarElement == grammarAccess.WSRule) {
				acceptor.accept(new CompletionProposal('''${}''', context.offset, 0, 2))
			}
		}
	}

	private def boolean isRuleCallTo(EObject grammarElement, AbstractRule rule) {
		if (grammarElement instanceof RuleCall) {
			return grammarElement.rule == rule
		}
		return false
	}

}
