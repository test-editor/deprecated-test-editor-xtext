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
package org.testeditor.tcl.dsl.jvmmodel

import com.google.inject.Inject
import java.util.Set
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tcl.SpecificationStep
import org.testeditor.tcl.TestStepContext
import static java.lang.System.lineSeparator
import org.testeditor.tcl.TestStep

class TclJvmModelInferrer extends AbstractModelInferrer {

	@Inject extension JvmTypesBuilder
	@Inject extension TclModelUtil

	def dispatch void infer(TclModel element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(element.toClass('''«element.package».«element.name»''')) [
			documentation = '''Generated from «element.eResource.URI»'''
			// Create variables for used fixture types
			for (fixtureType : element.fixtureTypes) {
				members += toField(element, fixtureType.fixtureFieldName, typeRef(fixtureType)) [
					initializer = '''new «fixtureType»()'''
				]
			}
			// Create test method
			members += element.toMethod('execute', typeRef(Void.TYPE)) [
				annotations += annotationRef('org.junit.Test')
				body = '''«element.generateMethodBody»'''
			]
		]
	}
	
	private def generateMethodBody(TclModel model) {
		return model.steps.map[generate].join(lineSeparator + lineSeparator)
	}
	
	private def generate(SpecificationStep step) '''
		/* 
		 * «step.contents.restoreString»
		 */
		«step.contexts.map[generate].join(lineSeparator)»
	'''
	
	private def generate(TestStepContext context) '''
		// Mask: «context.component.name»
		«FOR step : context.steps»
			// - «step.contents.restoreString»
			«step.toFeatureCall(context)»
		«ENDFOR»
	'''

	/**
	 * @return all {@link JvmType} of all fixtures that are referenced.
	 */
	private def Set<JvmType> getFixtureTypes(TclModel model) {
		val components = model.steps.map[contexts].flatten.map[component]
		// TODO the part from here should go somewhere in Aml ModelUtil
		val interactionTypes = components.map[type?.interactionTypes].filterNull.flatten.toSet
		val fixtureTypes = interactionTypes.map[defaultMethod?.typeReference?.type].filterNull.toSet
		return fixtureTypes
	}

	private def String getFixtureFieldName(JvmType fixtureType) {
		return fixtureType.simpleName.toFirstLower
	}
	
	private def toFeatureCall(TestStep step, TestStepContext context) {
		val interaction = step.getInteraction(context)
		if (interaction !== null) {
			// TODO implement
		} else {
			return '''
				// TODO could not resolve «context.component.name» - «step.contents.restoreString»
			'''
		}
	}

}

