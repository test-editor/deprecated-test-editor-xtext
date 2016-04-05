package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*

# CreateWebProjectWithExampleTest implements CreateSimpleTestEditorProject

* start creation process
Component: TestEditor
- Clean Workspace
- Restart Test-Editor with clean environment

Component: ProjectExplorer
- execute menu item  "New/Project..."  in tree <ProjektBaum>
- Protkolliere vorhandene UI Elemente

* Select type Test Editor Project
Component: NewDialog
- selektiere Element "Testeditor Project" in Baum <ProjectType>
- click on <NextButton>

* Give the Project the name "MyFirstProject"
// We must select the dialog again, because we are in a new TSL Step.
Component: NewDialog
- Type  "MyFirstWebProject" into <ProjectName>
- click on <NextButton>
- click on <NextButton>
- select element "Web Fixture" in List <AvailableFixturesList>
- click on <AddFixtureButton>
- click on <FinishButton>

* Verify that the project is visible in the ui
Component: HauptFenster
- selektiere Element "MyFirstWebProject" in Baum <ProjektBaum>
- Warte "1" Sekunden
