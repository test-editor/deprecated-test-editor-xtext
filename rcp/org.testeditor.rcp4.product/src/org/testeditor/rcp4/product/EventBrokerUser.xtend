package org.testeditor.rcp4.product

import javax.annotation.PostConstruct
import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Optional
import org.eclipse.e4.core.di.extensions.EventTopic
import org.eclipse.e4.core.services.events.IEventBroker

class EventBrokerUser{
	
	new(){
		System.out.println("EventBrokerUser is created")
	}

	@Inject
	var IEventBroker broker

	@PostConstruct	
	def send(){
		System.out.println("EventBrokerUser posted...")
		broker.post("hello", "data")
	}	


	@Inject
	@Optional
	def subscription(@EventTopic("hello") String data) {
		System.out.println('''EventBrokerUser received '«data»' on channel hello''')
	}
	
}