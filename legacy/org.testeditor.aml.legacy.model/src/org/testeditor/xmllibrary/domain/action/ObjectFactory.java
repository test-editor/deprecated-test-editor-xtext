//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4-2 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2013.08.30 at 10:58:28 AM CEST 
//

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
package org.testeditor.xmllibrary.domain.action;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.namespace.QName;

/**
 * This object contains factory methods for each Java content interface and Java
 * element interface generated in the org.testeditor.xmllibrary.domain.action
 * package.
 * <p>
 * An ObjectFactory allows you to programatically construct new instances of the
 * Java representation for XML content. The Java representation of XML content
 * can consist of schema derived interfaces and classes representing the binding
 * of schema type definitions, element declarations and model groups. Factory
 * methods for each of these are provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

	private static final QName _Value_QNAME = new QName("", "value");

	/**
	 * Create a new ObjectFactory that can be used to create new instances of
	 * schema derived classes for package:
	 * org.testeditor.xmllibrary.domain.action.
	 * 
	 */
	public ObjectFactory() {
	}

	/**
	 * Create an instance of {@link Argument }.
	 * 
	 * @return argument
	 */
	public Argument createArgument() {
		return new Argument();
	}

	/**
	 * Create an instance of {@link ActionGroups }.
	 * 
	 * @return action groups
	 */
	public ActionGroups createActionGroups() {
		return new ActionGroups();
	}

	/**
	 * Create an instance of {@link ActionGroup }.
	 * 
	 * @return action group
	 */
	public ActionGroup createActionGroup() {
		return new ActionGroup();
	}

	/**
	 * Create an instance of {@link ActionName }.
	 * 
	 * @return action name
	 */
	public ActionName createActionName() {
		return new ActionName();
	}

	/**
	 * Create an instance of {@link Action }.
	 * 
	 * @return new action
	 */
	public Action createAction() {
		return new Action();
	}

	/**
	 * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code>}.
	 * 
	 * @param value
	 *            name value
	 * @return JAXB element
	 */
	@XmlElementDecl(namespace = "", name = "value")
	@XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	public JAXBElement<String> createValue(String value) {
		return new JAXBElement<String>(_Value_QNAME, String.class, null, value);
	}

}
