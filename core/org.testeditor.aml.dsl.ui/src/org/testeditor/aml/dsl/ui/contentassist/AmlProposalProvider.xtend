/*******************************************************************************
 * Copyright (c) 2012 - 2015 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.aml.dsl.ui.contentassist

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.jface.text.contentassist.CompletionProposal
import org.eclipse.xtext.AbstractRule
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.CrossReference
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor
import org.eclipse.xtext.ui.resource.ProjectByResourceProvider
import org.testeditor.aml.dsl.services.AmlGrammarAccess
import org.testeditor.aml.model.ElementTypeWithInteractions
import org.testeditor.aml.model.ElementWithInteractions

class AmlProposalProvider extends AbstractAmlProposalProvider {

	@Inject
	ProjectByResourceProvider projectByResourceProvider

	@Inject
	AmlGrammarAccess grammarAccess

	@Inject
	extension IQualifiedNameProvider

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

	override completeStringLiterals_Values(EObject model, Assignment assignment, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		super.completeStringLiterals_Values(model, assignment, context, acceptor)
		val node = context.currentNode
		if (node.grammarElement.isRuleCallTo(grammarAccess.STRINGRule)) {
			acceptor.accept(new CompletionProposal(''', ""''', node.offset + node.length, 0, 3))
		}
	}

	private def boolean isRuleCallTo(EObject grammarElement, AbstractRule rule) {
		if (grammarElement instanceof RuleCall) {
			return grammarElement.rule == rule
		}
		return false
	}

	/**
	 * Do not include value-space assignments that are already there in the proposal.
	 */
	override completeValueSpaceAssignment_Variable(EObject model, Assignment assignment, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		if (model instanceof ElementWithInteractions<?>) {
			val existingElements = model.valueSpaceAssignments.map[variable.fullyQualifiedName].toSet
			lookupCrossReference(assignment.terminal as CrossReference, context, acceptor) [
				return !existingElements.contains(qualifiedName)
			]
		}
	}

	/**
	 * @see completeElementTypeWithInteractions_InteractionTypes
	 */
	override completeComponentElementType_InteractionTypes(EObject model, Assignment assignment, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		completeElementTypeWithInteractions_InteractionTypes(model, assignment, context, acceptor)
	}

	/**
	 * @see completeElementTypeWithInteractions_InteractionTypes
	 */
	override completeComponentType_InteractionTypes(EObject model, Assignment assignment, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		completeElementTypeWithInteractions_InteractionTypes(model, assignment, context, acceptor)
	}

	/**
	 * Do not include interactions that are already there in the proposal.
	 */
	private def completeElementTypeWithInteractions_InteractionTypes(EObject model, Assignment assignment, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		if (model instanceof ElementTypeWithInteractions) {
			val existingElements = model.interactionTypes.map[fullyQualifiedName].toSet
			lookupCrossReference(assignment.terminal as CrossReference, context, acceptor) [
				return !existingElements.contains(qualifiedName)
			]
		}
	}

}
