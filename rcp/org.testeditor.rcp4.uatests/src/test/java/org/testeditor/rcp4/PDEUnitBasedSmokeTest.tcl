package org.testeditor.rcp4

import org.testeditor.rcp4.*

# PDEUnitBasedSmokeTest implements SmokeTest

config TestEditorConfig
	
* Validate initial views

	Component: MainWindow
	- Is view <ProjektBaum> visible
	- Is view <TestStepSelector> visible

	Component: TestEditorServices
	- Create simple project

	Component: ProjectExplorer
	- Select element "DemoProject" in tree <ProjectTree>

