package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*;

# CreateSimpleTestEditorProjectFromContextMenuWithNewWizardTest implements CreateSimpleTestEditorProject

Setup:

	Component: TestEditor
	- Clean Workspace
	- Reset UI State
	
* start creation process

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjektBaum>

* Select type Test Editor Project

	Component: NewDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>

* Give the Project the name "MyFirstProject"

	// We must select the dialog again, because we are in a new TSL Step.
	Component: NewDialog
	- Type "MyFirstProject" into <ProjectName>
	- Click on <FinishButton>
	- Wait until dialog with title "Progress Information" is closed

* Verify that the project is visible in the ui

	Component: ProjectExplorer
	- Select element "MyFirstProject" in tree <ProjektBaum>
