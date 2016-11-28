package org.testeditor.tcl.dsl.ui.editor

import org.eclipse.xtend.lib.annotations.Data
import org.testeditor.tcl.ComponentTestStepContext

@Data
class ModelInsertData {
	ComponentTestStepContext testStepContext
	int index
}