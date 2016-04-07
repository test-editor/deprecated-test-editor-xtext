package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*

# CreateWebProjectWithExampleTest implements CreateSimpleTestEditorProject

* start creation process
Component: TestEditor
- Clean Workspace
- Reset UI State

Component: ProjectExplorer
- execute menu item  "New/Project..."  in tree <ProjektBaum>

* Select type Test Editor Project
Component: NewDialog
- Select element "Testeditor Project" in tree <ProjectType>
- Click on <NextButton>

* Give the Project the name "MyFirstProject"
// We must select the dialog again, because we are in a new TSL Step.
Component: NewDialog
- Type  "MyFirstWebProject" into <ProjectName>
- Click on <NextButton>
- Click on <NextButton>
- select element "Web Fixture" in List <AvailableFixturesList>
- Click on <AddFixtureButton>
- Click on <FinishButton>

* Verify that the project is visible in the ui
Component: ProjectExplorer
- Select element "MyFirstWebProject" in tree <ProjektBaum>
