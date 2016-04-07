package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*

# CreateWebProjectWithExampleTest implements CreateSimpleTestEditorProject

* start creation process
Component: TestEditor
- Clean Workspace
- Reset UI State

Component: ProjectExplorer
- execute menu item  "New/Project..."  in tree <ProjektBaum>

Component: NewDialog
 - Click on <NextButton>

* Select type Test Editor Project
Component: NewDialog
- Select element "Testeditor Project" in tree <ProjectType>
- click on <NextButton>

* Give the Project the name "MyFirstProject"
// We must select the dialog again, because we are in a new TSL Step.
Component: NewDialog
- Type  "MyFirstWebProject" into <ProjectName>
- click on <NextButton>
- click on <NextButton>
- select element "Web Fixture" in List <AvailableFixturesList>
- click on <AddFixtureButton>
- wait "5" seconds
- click on <FinishButton>

* Verify that the project is visible in the ui
Component: ProjectExplorer
- Select element "MyFirstWebProject" in tree <ProjektBaum>
