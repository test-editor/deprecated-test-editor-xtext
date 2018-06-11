package org.testeditor.tcl.dsl.ide

import com.google.common.base.Predicate
import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.CrossReference
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.Keyword
import org.eclipse.xtext.RuleCall
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ide.editor.contentassist.IIdeContentProposalAcceptor
import org.eclipse.xtext.ide.editor.contentassist.IdeContentProposalProvider
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.IEObjectDescription
import org.eclipse.xtext.xtext.CurrentTypeFinder
import org.slf4j.LoggerFactory
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateContainer
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.dsl.services.TclGrammarAccess
import org.testeditor.tcl.util.TclModelUtil

class TclContentProposalProvider extends IdeContentProposalProvider {

	static val logger = LoggerFactory.getLogger(TclContentProposalProvider)

	@Inject JvmTypeReferenceUtil typeUtil
	@Inject SimpleTypeComputer typeComputer
	@Inject extension TclGrammarAccess
	@Inject extension TclModelUtil
	@Inject extension ModelUtil
	@Inject CurrentTypeFinder currentTypeFinder

	/**
	 * hook into proposal construction and identify relevant cases
	 */
	override protected _createProposals(RuleCall ruleCall, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val contextTrimmedPrefix = context.copy => [
			val lastCompletedNodeText = context.lastCompleteNode.text.trim
			prefix = if (context.prefix.trim.isEmpty && lastCompletedNodeText == '-') {
				''
			} else {
				context.prefix
			}
		]
		switch (ruleCall.rule) {
			case testStepRule:
				makeTestStepProposals(contextTrimmedPrefix.toContext, acceptor)
			case innerStepContentRule,
			case stepContentElementRule:
				makeStepContentElementProposals(contextTrimmedPrefix.toContext, acceptor)
			case IDRule:
				makeIDProposals(contextTrimmedPrefix.toContext, acceptor)
			default: {
				// no proposals for other rules (yet), standard proposals (derived by the grammar) are added nonetheless
			}
		}
	}

	override protected _createProposals(Keyword keyword, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val contextTrimmedPrefix = context.copy => [prefix = context.prefix.trim]
		super._createProposals(keyword, contextTrimmedPrefix.toContext, acceptor)
	}

	override protected _createProposals(Assignment assignment, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val contextTrimmedPrefix = context.copy => [prefix = context.prefix.trim]
		super._createProposals(assignment, contextTrimmedPrefix.toContext, acceptor)
	}

	/**
	 * filter all proposals for cross references such that TemplateVariables may not be used in VariableReferencePath location!
	 */
	override protected def Predicate<IEObjectDescription> getCrossrefFilter(CrossReference reference, ContentAssistContext context) {
		val type = currentTypeFinder.findCurrentTypeAfter(reference)
		if (type.instanceClass == VariableReferencePathAccess) {
			return [ eObjectDescription |
				val eObject = eObjectDescription.EObjectOrProxy
				return !(eObject instanceof TemplateVariable)
			]
		} else {
			return super.getCrossrefFilter(reference, context)
		}
	}

	private def void makeTestStepProposals(ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val entry = proposalCreator.createProposal('- ', context)
		val prio = proposalPriorities.getDefaultPriority(entry)
		acceptor.accept(entry, prio)
		if (context.lastCompleteNode.text.endsWith('\n')) {
			val prevLeafNode = NodeModelUtils.findLeafNodeAtOffset(context.currentNode.previousSibling, context.lastCompleteNode.offset - 1)
			var modelElement = NodeModelUtils.findActualSemanticObjectFor(prevLeafNode)
			val testStepContext = modelElement.testStepContext
			val templates = switch (testStepContext) {
				ComponentTestStepContext: testStepContext.component?.allInteractionTypes.map[template]
				MacroTestStepContext: testStepContext.macroCollection.macros.map[template]
				default: #[]
			}
			templates.forEach[makeInitialTemplateProposalWithDash(context, acceptor)]
		}
	}

	private def void makeInitialTemplateProposalWithDash(Template template, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val proposal = '''- «template.restoreString»''' // NOTE THAT - IS EXPLICITLY INCLUDED HERE
		val templateEntry = proposalCreator.createProposal(proposal, context)
		if (templateEntry !== null) {
			acceptor.accept(templateEntry, proposalPriorities.getCrossRefPriority(null, templateEntry))
		}
	}

	private def void makeStepContentElementProposals(ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val model = context.currentModel
		if (model instanceof TestStepContext) {
			model.envParams.forEach[makeEnvParamProposal(context, acceptor)]
			model.enclosingMacroParameters.forEach[makeMacroParameterProposal(context, acceptor)]
			if (context.previousModel.isIncompleteTestStep) {
				if (model instanceof ComponentTestStepContext) {
					model.component?.makeComponentTemplateProposal(context, acceptor)
				}
				if (model instanceof MacroTestStepContext) {
					model.macroCollection?.makeMacroTemplateProposal(context, acceptor)
				}
			}
		} else if (model instanceof TestStep) {
			model.envParams.forEach[makeEnvParamProposal(context, acceptor)]
			model.enclosingMacroParameters.forEach[makeMacroParameterProposal(context, acceptor)]
			if (model.componentContext !== null) { // since the macro context does not allow for element parameter, yet, only component contexts are relevant here
				model.makeElementParamsProposal('', context, acceptor)
			}
			if (context.previousModel.isIncompleteTestStep) {
				model.componentContext?.component?.makeComponentTemplateProposal(context, acceptor)
				model.macroContext?.macroCollection?.makeMacroTemplateProposal(context, acceptor)
			}
		} else if (model instanceof StepContentElement) {
			model.makeElementParamsProposal(context, acceptor)
		}
	}

	/**
	 * if a test step is a complete reference to a fixture, it is probably safe to asume that nothing additional should
	 * be added to this very teststep. 
	 * 
	 * actually the last parsed element (previousModel) is tested whether it is part of a fully parsed test step
	 */
	private def boolean isIncompleteTestStep(EObject previousModel) {
		EcoreUtil2.getContainerOfType(previousModel, TestStep).templateContainer === null
	}

	private def void makeIDProposals(ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val model = context.currentModel
		if (model instanceof StepContentElement) {
			model.makeElementParamsProposal(context, acceptor)
		}
	}

	private def makeEnvParamProposal(EnvironmentVariable environmentVariable, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val envName = environmentVariable.name
		val proposal = proposalCreator.createProposal(context.prefix + "@" + envName, context)
		if (proposal !== null) {
			acceptor.accept(proposal => [
				label = envName
				description = 'ENV_PARAM'
			], proposalPriorities.getCrossRefPriority(null, proposal))
		}
	}

	private def makeMacroParameterProposal(TemplateVariable templateVariable, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val proposal = proposalCreator.createProposal(context.prefix + "@" + templateVariable.name, context)
		acceptor.accept(proposal, proposalPriorities.getCrossRefPriority(null, proposal))
	}

	private def makeComponentTemplateProposal(Component component, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val interactions = component.allInteractionTypes
		interactions.map[template].makeTemplateProposals(context, acceptor)
	}

	private def makeMacroTemplateProposal(MacroCollection macroCollection, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val macros = macroCollection.macros
		macros.map[template].makeTemplateProposals(context, acceptor)
	}

	private def makeTemplateProposals(Iterable<Template> templates, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val mandatoryPrefix = context.mandatoryPrefixForTemplate(true)
		templates.forEach [
			makeTemplateProposal(mandatoryPrefix, context, acceptor)
		]
	}

	private def makeTemplateProposal(Template template, String mandatoryPrefix, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val element = template.restoreString
		if (element.startsWith(mandatoryPrefix)) {
			val actualElement = context.prefix + element.substring(mandatoryPrefix.length).trim
			val proposal = proposalCreator.createProposal(actualElement, context)
			if (proposal !== null) {
				acceptor.accept(proposal => [
					label = element
					description = 'STEP'
				], proposalPriorities.getCrossRefPriority(null, proposal))
			}
		}
	}

	private def makeElementParamsProposal(StepContentElement element, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		val model = EcoreUtil2.getContainerOfType(element, TestStep)
		model.makeElementParamsProposal(element.value, context, acceptor)
	}

	/**
	 * return elements that are available for the given interaction and have the given prefix
	 */
	private def Iterable<ComponentElement> filterElementsApplicable(Iterable<ComponentElement> elements, InteractionType interaction, String prefix) {
		return elements.filter [
			val interactionTypes = componentElementInteractionTypes
			val validCandidate = interactionTypes.contains(interaction) && name.startsWith(prefix)
			if (logger.traceEnabled) {
				logger.trace('''Element-Parameter validForProposal='«validCandidate»' name='«name»'.''')
			}
			return validCandidate
		]
	}

	private def makeElementParamsProposal(TestStep model, String prefix, ContentAssistContext context, IIdeContentProposalAcceptor acceptor) {
		if (!context.prefix.isNullOrEmpty) {
			val componentContext = EcoreUtil2.getContainerOfType(model, ComponentTestStepContext)
			val component = componentContext.component
			val interaction = model.interaction
			val applicableElements = component.elements.filterElementsApplicable(interaction, prefix)
			val currentNode = context.currentNode
			val includeClosingBracket = !currentNode.text.contains('>') && (currentNode.nextSibling === null || !currentNode.nextSibling.text.contains('>'))
			applicableElements.forEach [ el |
				val entry = '''«IF prefix.isNullOrEmpty»<«ENDIF»«el.name»«IF includeClosingBracket»>«ENDIF»'''
				if (logger.traceEnabled) {
					logger.trace('''Element-Parameter-Proposal name='«el.name»', context.prefix='«context.prefix»', proposal entry='«entry»'.''')
				}
				val proposal = proposalCreator.createProposal(entry, context)
				if (proposal !== null) {
					acceptor.accept(proposal => [
						proposal = '''«el.name»«IF includeClosingBracket»>«ENDIF»'''
						label = el.name
						description = 'ELEMENT'
					], proposalPriorities.getCrossRefPriority(null, proposal))
				}
			]
		}
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

	private def String peekBehindContextCursor(ContentAssistContext context, int length) {
		val peekOffset = Math.max(0, context.offset - length)
		val peekedString = context.rootNode.text.substring(peekOffset, context.offset)
		return peekedString
	}

	private def String peekBehindSuchThatPrefixForTemplateIsFullyCaptured(ContentAssistContext context) {
		return context.peekBehindContextCursor(200) // longer fixture templates should not exist 
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
		val spacedPrefix = denormalizedPrefix.replaceAll('\t', ' ')
		return spacedPrefix.replaceAll(' +', ' ').replaceAll('^ ', '')
	}

	private def String mandatoryPrefixForTemplate(ContentAssistContext context, boolean normalize) {
		val peekedString = context.peekBehindSuchThatPrefixForTemplateIsFullyCaptured
		val relevantPeekedString = peekedString.reduceStringToTemplatePrefixRelevantPart
		if (normalize) {
			return relevantPeekedString.normalizeTemplatePrefix
		} else {
			return relevantPeekedString
		}
	}

}
