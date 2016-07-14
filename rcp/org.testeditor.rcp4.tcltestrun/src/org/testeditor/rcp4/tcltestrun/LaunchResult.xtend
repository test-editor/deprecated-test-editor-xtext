package org.testeditor.rcp4.tcltestrun

import java.io.File
import org.eclipse.xtend.lib.annotations.Data

@Data class LaunchResult {
	File expectedFileRoot
	Integer returnCode
	Exception exception // may be null
}
