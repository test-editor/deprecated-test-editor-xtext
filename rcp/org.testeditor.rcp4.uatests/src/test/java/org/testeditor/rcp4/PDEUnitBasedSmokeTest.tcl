package org.testeditor.rcp4

import org.testeditor.rcp4.*

# PDEUnitBasedSmokeTest implements SmokeTest

config TestEditorConfig
	
* Validate initial views

	Component: HauptFenster
	- Is view <ProjektBaum> visible
	- Is view <TestStepSelector> visible

	Component: TestEditor
	- Create simple project

	Component: ProjectExplorer
	- Select element "DemoProject" in tree <ProjektBaum>

