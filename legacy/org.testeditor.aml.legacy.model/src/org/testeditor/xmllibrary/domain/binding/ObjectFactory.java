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
package org.testeditor.xmllibrary.domain.binding;

import javax.xml.bind.annotation.XmlRegistry;


/**
 * This object contains factory methods for each
 * Java content interface and Java element interface
 * generated in the org.testeditor.xmllibrary.domain.binding package.
 * <p>An ObjectFactory allows you to programatically
 * construct new instances of the Java representation
 * for XML content. The Java representation of XML
 * content can consist of schema derived interfaces
 * and classes representing the binding of schema
 * type definitions, element declarations and model
 * groups.  Factory methods for each of these are
 * provided in this class.
 *
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: org.testeditor.xmllibrary.domain.binding
     *
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link TechnicalBindingTypes }
     *
     */
    public TechnicalBindingTypes createTechnicalBindingTypes() {
        return new TechnicalBindingTypes();
    }

    /**
     * Create an instance of {@link TechnicalBindingType }
     *
     */
    public TechnicalBindingType createTechnicalBindingType() {
        return new TechnicalBindingType();
    }

    /**
     * Create an instance of {@link ActionPart }
     *
     */
    public ActionPart createActionPart() {
        return new ActionPart();
    }

}
