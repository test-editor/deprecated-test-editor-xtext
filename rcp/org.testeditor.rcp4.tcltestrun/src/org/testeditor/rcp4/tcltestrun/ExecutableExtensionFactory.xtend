package org.testeditor.rcp4.tcltestrun

import org.osgi.framework.Bundle

class ExecutableExtensionFactory extends org.testeditor.dsl.common.ui.utils.ExecutableExtensionFactory {

	override Bundle getBundle() {
		return Activator.^default.bundle
	}

}
