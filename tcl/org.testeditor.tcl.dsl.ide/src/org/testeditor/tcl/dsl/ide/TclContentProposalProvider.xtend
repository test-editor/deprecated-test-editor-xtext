package org.testeditor.tcl.dsl.ide

import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ide.editor.contentassist.IIdeContentProposalAcceptor
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalProvider
import org.testeditor.aml.Component
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateContainer
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.dsl.services.TclGrammarAccess
import org.testeditor.tcl.util.TclModelUtil

class TclContentProposalProvider extends IdeContentProposalProvider {

	// static val logger = LoggerFactory.getLogger(TclContentProposalProvider)
	@Inject JvmTypeReferenceUtil typeUtil
	@Inject SimpleTypeComputer typeComputer
	@Inject extension TclGrammarAccess grammarAccess
	@Inject extension TclModelUtil
	@Inject extension ModelUtil

	private def makeEnvParamsProposal(TestStep model, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		model.envParams.forEach [
			val proposal = proposalCreator.createProposal("@" + name, context)
			if (proposal !== null) {
				acceptor.accept(proposal, proposalPriorities.getCrossRefPriority(null, proposal))
			}
		]
	}

	private def makeMacroParameterProposal(TestStep model, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		model.enclosingMacroParameters.forEach [
			val proposal = proposalCreator.createProposal("@" + name, context)
			if (proposal !== null) {
				acceptor.accept(proposal, proposalPriorities.getCrossRefPriority(null, proposal))
			}
		]
	}

	private def makeComponentTemplateProposal(Component component, TestStep model, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val interaction = model.interaction
		val possibleElements = component.elements.filter [
			val interactionTypes = componentElementInteractionTypes
			return interactionTypes.contains(interaction)
		]
		// need to consider whether the completion should contain the '>' as well
		val currentNode = context.currentNode
		val includeClosingBracket = !currentNode.text.contains('>') && (currentNode.nextSibling === null || !currentNode.nextSibling.text.contains('>'))
		possibleElements.forEach [
			val element = '''<«name»«IF includeClosingBracket»>«ENDIF»'''
			val proposal = proposalCreator.createProposal(element, context)
			if (proposal !== null) {
				acceptor.accept(proposal, proposalPriorities.getCrossRefPriority(null, proposal))
			}
		]

		val interactions = component.allInteractionTypes
		interactions.map[template].makeTemplateProposals(context, acceptor)
	}

	private def makeMacroTemplateProposal(MacroCollection macroCollection, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val macros = macroCollection.macros
		macros.map[template].makeTemplateProposals(context, acceptor)
	}

	private def makeTemplateProposals(Iterable<Template> templates, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val mandatoryPrefix = context.mandatoryPrefixForTemplate
		val rightBehindSpace = context.rightBehindSpace
		templates.forEach [
			makeTemplateProposal(mandatoryPrefix, rightBehindSpace, context, acceptor)
		]
	}

	private def makeTemplateProposal(Template template, String mandatoryPrefix, boolean rightBehindSpace, ContentAssistContext context,
		IIdeContentProposalAcceptor acceptor) {
		val element = template.restoreString
		if (element.startsWith(mandatoryPrefix)) {
			val prefixedContext = context.copy.setPrefix(mandatoryPrefix.addSpaceIfNotEmptyAnd(rightBehindSpace)).toContext
			val proposal = proposalCreator.createProposal(element, prefixedContext)
			if (proposal !== null) {
				acceptor.accept(proposal, proposalPriorities.getCrossRefPriority(null, proposal))
			}
		}
	}

	override protected _createProposals(RuleCall ruleCall, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		if (ruleCall.rule === grammarAccess.testStepRule) {
			val entry = proposalCreator.createProposal('- ', context)
			val prio = proposalPriorities.getKeywordPriority(null, entry)
			if (entry !== null) {
				acceptor.accept(entry, prio)
			}
		} else if (ruleCall.rule === grammarAccess.stepContentElementRule) {
			val model = context.currentModel
			if (model instanceof TestStep) {
				model.makeEnvParamsProposal(context, acceptor)
				model.makeMacroParameterProposal(context, acceptor)
				model.componentContext?.component?.makeComponentTemplateProposal(model, context, acceptor)
				model.macroContext?.macroCollection?.makeMacroTemplateProposal(context, acceptor)
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
				return '"1"'
			} else if (typeUtil.isEnum(expectedType.get)) {
				return typeUtil.getEnumValues(expectedType.get).head
			} else if (typeUtil.isBoolean(expectedType.get)) {
				return '"true"'
			}
		}

		// default
		return '''"«templateVariable.name»"'''
	}

	/**
	 * return grammarElement of given node as 'clazz'
	 * <pre>
	 * iff: the node is a leaf node (thus not null)
	 *      the grammarElement is of the requested type
	 * else: null
	 * </pre>
	 */
	/*
	 * private def <T> getNodeAsGrammarElement(INode node, Class<T> clazz) {
	 * 	if ((node !== null) && (node instanceof LeafNode)) {
	 * 		val grammarElement = (node as LeafNode).grammarElement
	 * 		if (clazz.isAssignableFrom(grammarElement.class)) {
	 * 			return grammarElement as T
	 * 		}
	 * 	} 
	 * 	return null
	 * }
	 * 
	 * private def boolean isRightBehindDashToken(ContentAssistContext context) {
	 * 	val lastCompleteKeyword = context.lastCompleteNode.getNodeAsGrammarElement(Keyword)
	 * 	return lastCompleteKeyword !== null && lastCompleteKeyword.value == '-'
	 * }

	 * private def boolean isRightBehindAssignmentToken(ContentAssistContext context) {
	 * 	val lastCompleteKeyword = context.lastCompleteNode.getNodeAsGrammarElement(Keyword)
	 * 	return lastCompleteKeyword !== null && lastCompleteKeyword.value == '='
	 * }
	 */
	private def boolean isRightBehindSpace(ContentAssistContext context) {
		val peekedString = context.peekBehindContexCursor(1)
		return peekedString == ' '
	}

	private def String peekBehindContexCursor(ContentAssistContext context, int length) {
		val peekOffset = Math.max(0, context.offset - length)
		val peekedString = context.rootNode.text.substring(peekOffset, context.offset)
		return peekedString
	}

	private def String peekBehindSuchThatPrefixForTemplateIsFullyCaptured(ContentAssistContext context) {
		return context.peekBehindContexCursor(200) // longer fixture templates should not exist 
	}

	private def String reduceStringToTemplatePrefixRelevantPart(String peekedString) {
		val lastDashOffset = peekedString.lastIndexOf('-')
		// prefix for content completion will NOT support line breaks within the prefix, 
		// it will however support empty prefixes right after line breaks 
		// => cut off all before '-' and line breaks
		val lastLineFeedOffset = Math.max(peekedString.lastIndexOf('\r'), peekedString.lastIndexOf('\n'))
		val lastAssignmentOffset = peekedString.lastIndexOf('=')
		val relevantPeekedString = peekedString.substring(#[lastDashOffset + 1, lastLineFeedOffset + 1, lastAssignmentOffset + 1].max)
		return relevantPeekedString
	}

	private def String normalizeTemplatePrefix(String denormalizedPrefix) {
		return denormalizedPrefix.replaceAll('\t', ' ').replaceAll(' +', ' ').trim
	}

	private def String mandatoryPrefixForTemplate(ContentAssistContext context) {
		val peekedString = context.peekBehindSuchThatPrefixForTemplateIsFullyCaptured
		val relevantPeekedString = peekedString.reduceStringToTemplatePrefixRelevantPart
		val normalizedRelevantPeekedString = relevantPeekedString.normalizeTemplatePrefix
		return normalizedRelevantPeekedString
	}

	private def String addSpaceIfNotEmptyAnd(String original, boolean addSpace) {
		if (original.isNullOrEmpty || !addSpace) {
			return original
		} else {
			return '''«original» '''
		}
	}

}
