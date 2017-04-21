package org.testeditor.web;

import org.jboss.weld.environment.servlet.Listener;
import org.testeditor.tsl.dsl.web.TslServlet;

import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import io.swagger.jaxrs.listing.ApiListingResource;

public class TestEditorBackendApp extends Application<BackendConfiguration> {
	
	public static void main(final String[] args) throws Exception {
		new TestEditorBackendApp().run(args);
	}

	@Override
	public void initialize(Bootstrap<BackendConfiguration> bootstrap) {
	}
	
	@Override
	public void run(BackendConfiguration configuration, Environment environment) throws Exception {
		// Swagger documentation servlet 
		environment.jersey().register(new ApiListingResource());
		
		environment.servlets().addServlet("XtextServices", TslServlet.class);
		
		// Enable WELD support
		environment.servlets().addServletListeners(new Listener());
	}

}
