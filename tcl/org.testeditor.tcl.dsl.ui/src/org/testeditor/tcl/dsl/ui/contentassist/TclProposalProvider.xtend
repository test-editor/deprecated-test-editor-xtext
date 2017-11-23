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

import java.util.Optional
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.dsl.ui.contentassist.AmlProposalProvider
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentVariable

class TclProposalProvider extends AbstractTclProposalProvider {

	@Inject extension TclModelUtil
	@Inject extension ModelUtil
	@Inject AmlProposalProvider amlProposalProvider
	@Inject JvmTypeReferenceUtil typeUtil
	@Inject SimpleTypeComputer typeComputer

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
		if (model instanceof ComponentTestStepContext) {
			model.envParams.forEach [
				acceptor.accept(
					createCompletionProposal("@" + name, "@" + name + " // environment variable", null, context))
			]
			model.enclosingMacroParameters.forEach [
				acceptor.accept(createCompletionProposal("@" + name, "@" + name + " // macro parameter", null, context))
			]
		}
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
	
	/**
	 * provide content completion for value parameters that will be passed to fixtures expecting enums
	 */
	override completeStepContentText_Value(EObject model, Assignment assignment, ContentAssistContext context,
		ICompletionProposalAcceptor acceptor) {
		super.completeStepContentText_Value(model, assignment, context, acceptor)
		if (model instanceof TestStep) {
			val stepContentVariable = locateStepContentVariableOfValueParameterUnderCursor(model, context)
			val parameterType = stepContentVariable.expectedType(model.interaction)
			if (stepContentVariable.present && parameterType.present) {
				typeUtil.getEnumValues(parameterType.get).forEach [
					val displayString = '''«it» (type: «parameterType.get.qualifiedName»)'''
					val proposal = '''"«it»'''
					acceptor.accept(createCompletionProposal(proposal, displayString, null, context))
				]
			}
		}
	}

	/**
	 * Locate the step content variable that is under the cursor when using a value to pass.
	 * If not found, the optional is empty.
	 * 
	 * Example: Set port to "|8080", where | denotes the cursor position,
	 *          will yield the step content variable defining the number parameter, that is used to pass 8080 to the fixture
	 */
	private def Optional<StepContentVariable> locateStepContentVariableOfValueParameterUnderCursor(TestStep model,
		ContentAssistContext context) {
		val interaction = Optional.ofNullable(model.interaction)
		val previousElementIsText = context.previousModel instanceof StepContentText
		val cursorBehindDoubleQuote = context.prefix == '"'
		if (interaction.present && previousElementIsText && cursorBehindDoubleQuote) {
			val stepContent = model.contents.dropWhile[it !== context.previousModel].drop(1).head
			if (stepContent instanceof StepContentVariable) {
				return Optional.of(stepContent)
			}
		}
		return Optional.empty
	}

	/**
	 * get the expected type of the step content variable
	 */
	private def Optional<JvmTypeReference> expectedType(Optional<StepContentVariable> stepContentVariable,
		InteractionType interactionType) {
		if (stepContentVariable.present) {
			return typeComputer.getExpectedType(stepContentVariable.get, interactionType)
		}
		return Optional.empty
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
