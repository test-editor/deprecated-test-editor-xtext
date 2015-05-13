package org.testeditor.xmllibrary.dsl.importer.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;
import org.testeditor.xmllibrary.dsl.importer.ImporterModule;

import com.google.inject.Guice;
import com.google.inject.Injector;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	public static final String PLUGIN_ID = "org.testeditor.xmllibrary.dsl.importer.ui"; //$NON-NLS-1$

	private static Activator plugin;
	
	private Injector injector;

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		injector = createInjector();
	}
	
	protected Injector createInjector() {
		return Guice.createInjector(new ImporterModule(), new ImporterUiModule());
	}
	
	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
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

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}
}
