package org.testeditor.rcp4

# CreateProjectOverServiceTest implements SmokeTest

Setup:

	Component: TestEditor
	- Clean Workspace
	- Reset UI State

* Validate initial views

	Component: TestEditor
	- Create simple project

	Component: ProjectExplorer
	- Select element "DemoProject" in tree <ProjektBaum>
