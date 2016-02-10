package org.testeditor.rcp4.product

import javax.annotation.PostConstruct
import javax.inject.Inject
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
