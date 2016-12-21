/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
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
package org.testeditor.tcl.dsl.ui.contentassist

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.dsl.ui.contentassist.AmlProposalProvider
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.util.TclModelUtil

class TclProposalProvider extends AbstractTclProposalProvider {

	@Inject extension TclModelUtil
	@Inject extension ModelUtil
	@Inject AmlProposalProvider amlProposalProvider

	override completeTestCase_Steps(EObject model, Assignment assignment, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		// a proposal of missing steps is not trivial, since the order of the existing/missing steps must
		// be taken into account. the completion of steps should be done via (quick) fix.
		// this completion proposal will allow for steps to be entered even though no "implements" is given yet.
		acceptor.accept(createCompletionProposal('* ', '* test description', null, context))
	}

	override complete_TestStepContext(EObject model, RuleCall ruleCall, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		// TODO this should be done using an auto-editing feature
		acceptor.accept(createCompletionProposal('Mask: ', 'Mask:', getImage(model), context))
		acceptor.accept(createCompletionProposal('Component: ', 'Component:', getImage(model), context))
	}

	override complete_TestStep(EObject model, RuleCall ruleCall, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		acceptor.accept(createCompletionProposal('- ', '- test step', null, context))
	}

	override complete_StepContentElement(EObject model, RuleCall ruleCall, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		super.complete_StepContentElement(model, ruleCall, context, acceptor)
		if (model instanceof TestStep) {
			model.envParams.forEach [
				acceptor.accept(
					createCompletionProposal("@" + name, "@" + name + " // environment variable", null, context))
			]
			model.enclosingMacroParameters.forEach [
				acceptor.accept(createCompletionProposal("@" + name, "@" + name + " // macro parameter", null, context))
			]
			val interaction = model.interaction
			val component = model.componentContext?.component
			if (component != null) {
				val possibleElements = component.elements.filter [
					val interactionTypes = componentElementInteractionTypes
					return interactionTypes.contains(interaction)
				]
				// need to consider whether the completion should contain the '>' as well
				val currentNode = context.currentNode
				val includeClosingBracket = !currentNode.text.contains('>') &&
					(currentNode.nextSibling === null || !currentNode.nextSibling.text.contains('>'))
				possibleElements.forEach [
					val displayString = '''«name»«IF !label.nullOrEmpty» - "«label»"«ENDIF» (type: «type.name»)'''
					val proposal = '''<«name»«IF includeClosingBracket»>«ENDIF»'''
					acceptor.accept(createCompletionProposal(proposal, displayString, image, context))
				]
			}
		}
	}
	
	/** Proposals for macro templates => should be same as in AML */
	override complete_TemplateText(EObject model, RuleCall ruleCall, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		amlProposalProvider.complete_TemplateText(model, ruleCall, context, acceptor)
	}

	/** Proposals for macro templates => should be same as in AML */
	override complete_TemplateVariable(EObject model, RuleCall ruleCall, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		amlProposalProvider.complete_TemplateVariable(model, ruleCall, context, acceptor)
	}

}
