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
import org.eclipse.jface.text.templates.ContextTypeRegistry
import org.eclipse.jface.text.templates.TemplateContext
import org.eclipse.jface.text.templates.persistence.TemplateStore
import org.eclipse.xtext.Keyword
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.nodemodel.impl.LeafNode
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ITemplateAcceptor
import org.eclipse.xtext.ui.editor.templates.ContextTypeIdHelper
import org.eclipse.xtext.ui.editor.templates.DefaultTemplateProposalProvider
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateContainer
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.dsl.common.util.JvmTypeReferenceUtil
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.dsl.jvmmodel.SimpleTypeComputer
import org.testeditor.tcl.util.TclModelUtil

class TclTemplateProposalProvider extends DefaultTemplateProposalProvider {
	@Inject extension ModelUtil
	@Inject extension TclModelUtil
	@Inject IQualifiedNameProvider nameProvider
	@Inject JvmTypeReferenceUtil typeUtil
	@Inject SimpleTypeComputer typeComputer

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
		if (model instanceof ComponentTestStepContext) {
			model?.proposeAvailableInteractions(templateContext, context, acceptor)
		}
		if (model instanceof TestStep) {
			if (model.hasComponentContext) {
				val componentContext = model.componentContext
				componentContext?.proposeAvailableInteractions(templateContext, context, acceptor)
			}else if (model.hasMacroContext){
				val macro = model.macroContext
				macro?.proposeAvailableInteractions(model, templateContext, context, acceptor)
			}
		}
	}
	
	/**
	 * give this' node grammarElement as 'clazz'
	 * <pre>
	 * iff: the node is a leaf node (thus not null)
	 *      the grammarElement is of the requested type
	 * else: null
	 * </pre>
	 */
	private def <T> getNodeAsGrammarElement(INode node, Class<T> clazz) {
		if ((node !== null) && (node instanceof LeafNode)) {
			val grammarElement = (node as LeafNode).grammarElement
			if (clazz.isAssignableFrom(grammarElement.class)) {
				return grammarElement as T
			}
		} 
		return null
	}
	
	private def boolean isRightBehindDash(ContentAssistContext context) {
		val lastCompleteKeyword = context.lastCompleteNode.getNodeAsGrammarElement(Keyword)
		return lastCompleteKeyword !== null && lastCompleteKeyword.value == '-'
	}

	private def boolean rightBehindSpace(ContentAssistContext context) {
		val peekedString = context.peekBehindContexCursor(1)
		return peekedString == ' '
	}
	
	private def String peekBehindContexCursor(ContentAssistContext context, int length) {
		val peekOffset = Math.max(0, context.offset - length)
		val peekedString = context.document.get(peekOffset, context.offset - peekOffset)
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
		val relevantPeekedString = peekedString.substring(Math.max(lastDashOffset + 1, lastLineFeedOffset + 1))
		return relevantPeekedString
	}
	
	private def String normalizeTemplatePrefix(String denormalizedPrefix) {
		return denormalizedPrefix.replaceAll(' +', ' ').replaceAll('/t', ' ').trim
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
		}else{
			return '''«original» '''
		}
	}
	
	private def Iterable<InteractionType> filterNotApplicableToUnknownElements(Iterable<InteractionType> originalList,
		ComponentTestStepContext testStepContext) {
		return originalList.filter [ interactionType |
			val hasElementParameter = interactionType.template.contents.filter(TemplateVariable).exists [
				name == 'element'
			]
			return !hasElementParameter || testStepContext.component.elements.exists [
				componentElementInteractionTypes.contains(interactionType)
			]
		]
	}
	
	private def Iterable<InteractionType> filterWithMatchingTemplatePrefix(Iterable<InteractionType> originalList,
		String prefix) {
		return originalList.filter [
			val restoredString = template.restoreString(false, it)
			restoredString.startsWith(prefix)
		]
	}
	
	def void proposeAvailableInteractions(ComponentTestStepContext testStepContext, TemplateContext templateContext,
		ContentAssistContext context, ITemplateAcceptor acceptor) {
		val mandatoryPrefix = context.mandatoryPrefixForTemplate
		val rightBehindSpace = context.rightBehindSpace
		// if a mandatory prefix is given, context replaceRegion must be present in order to not have duplicate proposals
		// if behind a space, replace region may be empty and prefix may be empty and but proposals may still exist 
		val contextIsRelevantForProposal = (context.replaceRegion.length > 0 || mandatoryPrefix.isNullOrEmpty || rightBehindSpace)
		if (contextIsRelevantForProposal) {
			val interactionTypes = testStepContext.component.allInteractionTypes
			val interactionTypesMatchingPrefix = interactionTypes.filterWithMatchingTemplatePrefix(mandatoryPrefix.addSpaceIfNotEmptyAnd(rightBehindSpace))
			val interactionsThatAreApplicableInThisComponent = interactionTypesMatchingPrefix.filterNotApplicableToUnknownElements(testStepContext)
			interactionsThatAreApplicableInThisComponent.forEach [
				val behindDash = context.isRightBehindDash
				val template = createTemplate(testStepContext, mandatoryPrefix, context.prefix, !behindDash)
				val relevance = 500
				// create proposal without validation, since validation fails on correct proposals because of misconceptions of replacement region and pasted text
				val proposal = doCreateProposal(template, templateContext, context, getImage(template), relevance) 
				acceptor.accept(proposal)
			]
		}
	}
	
	/**
	 * Provide dynamic templates for all available interaction types.
	 */
	def void proposeAvailableInteractions(MacroTestStepContext testStepContext, TestStep step, TemplateContext templateContext,
		ContentAssistContext context, ITemplateAcceptor acceptor) {
		val macros = testStepContext.macroCollection.macros
		macros.forEach [
			val template = createTemplate(testStepContext)
			val relevance = 500
			val proposal = createProposal(template, templateContext, context, getImage(template), relevance)
			acceptor.accept(proposal)
		]
	}

	/**
	 * Create a dynamic template for {@link InteractionType}.
	 */
	private def org.eclipse.jface.text.templates.Template createTemplate(InteractionType interactionType,
		ComponentTestStepContext context, String mandatoryPrefix, String contextPrefix, boolean withDash) {
		val proposalDescription = interactionType.template.restoreString(false, interactionType)
		val optionalDashPrefix = if (withDash && mandatoryPrefix.isNullOrEmpty) {
				'- '
			} else {
				''
			}
		val proposalTemplate = optionalDashPrefix + contextPrefix +
			interactionType.template.restoreString(true, interactionType).substring(mandatoryPrefix.length)
		val templateId = nameProvider.getFullyQualifiedName(interactionType).toString
		return new org.eclipse.jface.text.templates.Template(proposalDescription, 'test step', templateId,
			proposalTemplate.trim, false)
	}

	private def org.eclipse.jface.text.templates.Template createTemplate(Macro macro, MacroTestStepContext context) {
		val proposalDescription = macro.template.restoreString(false, macro)
		val proposalTemplate = macro.template.restoreString(true, macro)
		val templateId = nameProvider.getFullyQualifiedName(context.macroCollection)?.toString+"."+proposalDescription
		return new org.eclipse.jface.text.templates.Template(proposalDescription, 'test step', templateId, proposalTemplate, false)
	}

	private def String restoreString(Template template, boolean asTemplate, TemplateContainer templateContainer) {
		return template.contents.map [
			switch (it) {
				TemplateVariable case name == ModelUtil.TEMPLATE_VARIABLE_ELEMENT:
					if (asTemplate) '''<${«name»}>''' else '''<«name»>'''
				TemplateVariable:
					typedProposalString(templateContainer, asTemplate)
				TemplateText:
					value
			}
		].join(' ')
	}
	
	private def String typedProposalString(TemplateVariable templateVariable, TemplateContainer templateContainer, boolean asTemplate) {
		val expectedType = typeComputer.getExpectedType(templateVariable, templateContainer)
		if (expectedType.present) {
			typeUtil.initWith(templateVariable.eResource)
			if (typeUtil.isANumber(expectedType.get)) {
				return templateVariable.proposalStringValue("1", Optional.of(expectedType.get.simpleName), asTemplate)
			} else if (typeUtil.isEnum(expectedType.get)) {
				val value = typeUtil.getEnumValues(expectedType.get).head
				return templateVariable.proposalStringValue(value, Optional.of(expectedType.get.simpleName), asTemplate)
			}
		}
		
		// default
		return templateVariable.proposalStringValue(templateVariable.name, Optional.empty, asTemplate)
	}

	private def String proposalStringValue(TemplateVariable templateVariable, String value, Optional<String> typeInfo, boolean asTemplate) {
		val name = templateVariable.name
		if (asTemplate) {
			return '''"${«value»}"'''
		} else if (typeInfo.present) {
			return '''"«name»: «typeInfo.get»"'''
		} else{
			return '''"«value»"'''
		}
	}
	
}
