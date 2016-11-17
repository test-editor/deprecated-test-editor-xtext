package org.testeditor.rcp4.tcltestrun

import java.io.File
import org.eclipse.xtend.lib.annotations.Data

@Data
class LaunchResult {

	new(File expectedFileRoot) {
		this.expectedFileRoot = expectedFileRoot
		successful = true
		thrownException = null
	}

	new(File expectedFileRoot, boolean successful, Exception thrownException) {
		this.expectedFileRoot = expectedFileRoot
		this.successful = successful
		this.thrownException = thrownException
	}

	File expectedFileRoot
	boolean successful
	Exception thrownException

}
