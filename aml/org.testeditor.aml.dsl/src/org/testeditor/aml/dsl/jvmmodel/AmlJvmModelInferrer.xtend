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
package org.testeditor.aml.dsl.jvmmodel

import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.testeditor.aml.AmlModel
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import javax.inject.Inject
import org.eclipse.xtext.naming.IQualifiedNameProvider

// TODO this is not used, yet - instead we generate the old XML files for now
class AmlJvmModelInferrer extends AbstractModelInferrer {

    /**
     * convenience API to build and initialize JVM types and their members.
     */
	@Inject extension JvmTypesBuilder
	@Inject IQualifiedNameProvider nameProvider

	/**
	 * The dispatch method {@code infer} is called for each instance of the
	 * given element's type that is contained in a resource.
	 * 
	 * @param element
	 *            the model to create one or more
	 *            {@link JvmDeclaredType declared
	 *            types} from.
	 * @param acceptor
	 *            each created
	 *            {@link JvmDeclaredType type}
	 *            without a container should be passed to the acceptor in order
	 *            get attached to the current resource. The acceptor's
	 *            {@link IJvmDeclaredTypeAcceptor#accept(org.eclipse.xtext.common.types.JvmDeclaredType)
	 *            accept(..)} method takes the constructed empty type for the
	 *            pre-indexing phase. This one is further initialized in the
	 *            indexing phase using the closure you pass to the returned
	 *            {@linkplain IPostIndexingInitializing#initializeLater(org.eclipse.xtext.xbase.lib.Procedures.Procedure1)
	 *            initializeLater(..)}.
	 * @param isPreIndexingPhase
	 *            whether the method is called in a pre-indexing phase, i.e.
	 *            when the global index is not yet fully updated. You must not
	 *            rely on linking using the index if isPreIndexingPhase is
	 *            <code>true</code>.
	 */
   	def dispatch void infer(AmlModel element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
   		element.components.forEach[acceptor.accept(toInterface(nameProvider.getFullyQualifiedName(it).toString)[documentation = '''Generated from «element.eResource.URI»'''])[]]
   		element.componentTypes.forEach[acceptor.accept(toInterface(nameProvider.getFullyQualifiedName(it).toString)[documentation = '''Generated from «element.eResource.URI»'''])[]]
   		element.componentElementTypes.forEach[acceptor.accept(toInterface(nameProvider.getFullyQualifiedName(it).toString)[documentation = '''Generated from «element.eResource.URI»'''])[]]
   		element.interactionTypes.forEach[acceptor.accept(toInterface(nameProvider.getFullyQualifiedName(it).toString)[documentation = '''Generated from «element.eResource.URI»'''])[]]
   		element.valueSpaces.forEach[acceptor.accept(toInterface(nameProvider.getFullyQualifiedName(it).toString)[documentation = '''Generated from «element.eResource.URI»'''])[]]
   		// Here you explain how your model is mapped to Java elements, by writing the actual translation code.
   		
   		// An implementation for the initial hello world example could look like this:
//   		acceptor.accept(element.toClass("my.company.greeting.MyGreetings")) [
//   			for (greeting : element.greetings) {
//   				members += greeting.toMethod("hello" + greeting.name, typeRef(String)) [
//   					body = '''
//							return "Hello «greeting.name»";
//   					'''
//   				]
//   			}
//   		]
   	}
}

