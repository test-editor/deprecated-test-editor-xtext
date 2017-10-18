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
import org.eclipse.xtext.naming.IQualifiedNameProvider
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
import org.testeditor.dsl.common.util.TclJvmTypeReferenceUtil
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
	@Inject TclJvmTypeReferenceUtil typeUtil
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
		if (model instanceof TestStep) {
			if (model.hasComponentContext) {
				val componentContext = model.componentContext
				componentContext?.proposeAvailableInteractions(model, templateContext, context, acceptor)
			}else if (model.hasMacroContext){
				val macro = model.macroContext
				macro?.proposeAvailableInteractions(model, templateContext, context, acceptor)
			}
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
	 * Provide dynamic templates for all available interaction types.
	 */
	def void proposeAvailableInteractions(ComponentTestStepContext componentContext, TestStep step, TemplateContext templateContext,
		ContentAssistContext context, ITemplateAcceptor acceptor) {
		val interactionTypes = componentContext.component.allInteractionTypes
		interactionTypes.forEach [
			val template = createTemplate(componentContext)
			val relevance = 500
			val proposal = createProposal(template, templateContext, context, getImage(template), relevance)
			acceptor.accept(proposal)
		]
	}

	/**
	 * Create a dynamic template for {@link InteractionType}.
	 */
	private def org.eclipse.jface.text.templates.Template createTemplate(InteractionType interactionType, ComponentTestStepContext context) {
		val proposalDescription = interactionType.template.restoreString(false, interactionType)
		val proposalTemplate = '- ' + interactionType.template.restoreString(true, interactionType)
		val templateId = nameProvider.getFullyQualifiedName(interactionType).toString
		return new org.eclipse.jface.text.templates.Template(proposalDescription, 'test step', templateId, proposalTemplate, false)
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
