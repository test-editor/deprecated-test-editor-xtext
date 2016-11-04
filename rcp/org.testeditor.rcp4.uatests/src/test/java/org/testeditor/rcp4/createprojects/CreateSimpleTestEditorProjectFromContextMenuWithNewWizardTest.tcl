package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*;
import org.testeditor.fixture.swt.*

# CreateSimpleTestEditorProjectFromContextMenuWithNewWizardTest implements CreateSimpleTestEditorProject

config TestEditorConfig
	
* start creation process

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjectTree>

* Select type Test Editor Project

	Component: NewProjectDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>

* Give the Project the name "MyFirstProject"

	// We must select the dialog again, because we are in a new TSL Step.
	Component: NewProjectDialog
	- Type "MyFirstProject" into <ProjectName>
	- Click on <FinishButton>
	- Wait for dialog "Progress Information" to popup and then close after at most "3" respectively "120" seconds  

* Verify that the project is visible in the ui

	Component: ProjectExplorer
	- Select element "MyFirstProject" in tree <ProjectTree>
