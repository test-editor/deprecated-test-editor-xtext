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
package org.testeditor.rcp4

import javax.annotation.PostConstruct
import javax.inject.Inject
import org.apache.log4j.Level
import org.apache.log4j.Logger
import org.eclipse.e4.core.di.annotations.Optional
import org.eclipse.e4.core.di.extensions.EventTopic
import org.eclipse.e4.core.services.events.IEventBroker

/** example usage of the event broker in e4 */
class EventBrokerUser {

	private static final Logger logger = Logger.getLogger(EventBrokerUser);

	new() {
		logger.info("EventBrokerUser is created")
	}

	@Inject
	var IEventBroker broker

	@PostConstruct
	def void send() {
		logger.info("EventBrokerUser posted...")
		broker.post("hello", "data")
	}

	@Inject
	@Optional
	def void subscription(@EventTopic("hello") String data) {
		logger.info('''EventBrokerUser received '«data»' on channel hello''')
	}

}
