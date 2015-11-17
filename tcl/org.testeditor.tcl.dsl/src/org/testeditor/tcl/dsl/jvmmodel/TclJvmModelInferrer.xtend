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

import com.google.common.annotations.VisibleForTesting
import com.google.inject.Inject
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import org.testeditor.tcl.TclModel

class TclJvmModelInferrer extends AbstractModelInferrer {

	@Inject extension JvmTypesBuilder

   	def dispatch void infer(TclModel element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
   		val fileName = element.eResource.URI.lastSegment.removeFileExtension
   		
   		acceptor.accept(element.toClass('''«element.package».«fileName»''')) [
   			members += element.toMethod('performTest', typeRef(Void.TYPE))[
   				
   			]
   		]
   		
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
   	
   	@VisibleForTesting
   	protected def String removeFileExtension(String fileName) {
   		val separator = fileName.lastIndexOf('.')
   		if (separator >= 0) {
			return fileName.substring(0, separator)   			
   		} else {
   			return fileName
   		}
   	}
   	
}

