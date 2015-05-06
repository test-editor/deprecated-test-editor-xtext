package org.testeditor.xmllibrary.dsl.importer;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.google.inject.Guice;
import com.google.inject.Injector;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	public static final String PLUGIN_ID = "org.testeditor.xmllibrary.dsl.importer"; //$NON-NLS-1$

	private static Activator plugin;

	private Injector injector;
	
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		injector = createInjector();
	}

	protected Injector createInjector() {
		return Guice.createInjector(new ImporterModule());
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		injector = null;
		super.stop(context);
	}
	
	public Injector getInjector() {
		return injector;
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

}
