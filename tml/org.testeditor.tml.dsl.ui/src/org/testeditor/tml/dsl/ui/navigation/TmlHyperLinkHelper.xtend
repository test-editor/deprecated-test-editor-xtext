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
package org.testeditor.tml.dsl.ui.navigation

import javax.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.jface.text.Region
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.hyperlinking.IHyperlinkAcceptor
import org.eclipse.xtext.xbase.ui.navigation.XbaseHyperLinkHelper
import org.testeditor.aml.ComponentElement
import org.testeditor.dsl.common.ide.util.NodeRegionUtil
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.TestStep
import org.testeditor.tml.util.TmlModelUtil
import org.testeditor.tsl.StepContent

import static org.testeditor.tml.TmlPackage.Literals.*
import static org.testeditor.tsl.TslPackage.Literals.*

class TmlHyperLinkHelper extends XbaseHyperLinkHelper {

	@Inject extension NodeRegionUtil
	@Inject extension TmlModelUtil

	override createHyperlinksByOffset(XtextResource resource, int offset, IHyperlinkAcceptor acceptor) {
		super.createHyperlinksByOffset(resource, offset, acceptor)
		val element = getEObjectAtOffsetHelper.resolveElementAt(resource, offset)
		element.createHyperlinks(acceptor)
	}

	/**
	 * Create a hyperlink from a {@link TestStep} to its interaction's template.
	 */
	protected def dispatch void createHyperlinks(TestStep testStep, IHyperlinkAcceptor acceptor) {
		val interaction = testStep.interaction
		if (interaction !== null) {
			testStep.createHyperlinkTo(TEST_STEP__CONTENTS, interaction.template, acceptor)
		} else if (testStep.hasMacroContext) {
			val macroTemplate = testStep.macroContext?.findMacroDefinition?.template
			testStep.createHyperlinkTo(TEST_STEP__CONTENTS, macroTemplate, acceptor)
		}
	}

	/**
	 * Create a hyperlink from {@link StepContentElement} to its referenced {@link ComponentElement}.
	 *
	 * If it does not reference one, try to create a hyperlink for the eContainer at least.
	 */
	protected def dispatch void createHyperlinks(StepContentElement element, IHyperlinkAcceptor acceptor) {
		val componentElement = element.componentElement
		if (componentElement !== null) {
			element.createHyperlinkTo(STEP_CONTENT__VALUE, componentElement, acceptor)
		} else {
			element.eContainer.createHyperlinks(acceptor)
		}
	}

	/**
	 * Create a hyperlink for the eContainer.
	 */
	protected def dispatch void createHyperlinks(StepContent content, IHyperlinkAcceptor acceptor) {
		content.eContainer.createHyperlinks(acceptor)
	}

	/** For all other types, do nothing. */
	protected def dispatch void createHyperlinks(EObject eObject, IHyperlinkAcceptor acceptor) {
		// do nothing
	}

	protected def void createHyperlinkTo(EObject source, EStructuralFeature sourceFeature, EObject target,
		IHyperlinkAcceptor acceptor) {
		val textRegion = findNodesRegionForFeature(source, sourceFeature)
		if (textRegion !== null) {
			val resource = source.eResource as XtextResource
			val region = new Region(textRegion.offset, textRegion.length)
			createHyperlinksTo(resource, region, target, acceptor)
		}
	}

}
