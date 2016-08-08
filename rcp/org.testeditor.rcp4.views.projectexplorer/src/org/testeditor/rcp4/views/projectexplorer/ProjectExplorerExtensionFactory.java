package org.testeditor.rcp4.views.projectexplorer;

import org.eclipse.xtext.ui.guice.AbstractGuiceAwareExecutableExtensionFactory;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import com.google.inject.Guice;
import com.google.inject.Injector;

public class ProjectExplorerExtensionFactory extends AbstractGuiceAwareExecutableExtensionFactory {

	@Override
	protected Bundle getBundle() {
		return FrameworkUtil.getBundle(this.getClass());
	}

	@Override
	protected Injector getInjector() {
		return Guice.createInjector();
	}

}
