package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*;

#CreateSimpleTestEditorProjectFromContextMenuWithNewWizard implements CreateSimpleTestEditorProject

* start creation process
Component: ProjectExplorer
- execute menu item  "New/Project..."  in tree <ProjektBaum>
- Protkolliere vorhandene UI Elemente


* Select type Test Editor Project
Component: NewDialog
- selektiere Element "Other/Testeditor Project" in Baum <ProjectType>
- click on <NextButton>

* Give the Project the name "MyFirstProject"
// We must select the dialog again, because we are in a new TSL Step.
Component: NewDialog
- Type  "MyFirstProject"  into <ProjectName>
- click on <FinishButton>

* Verify that the project is visible in the ui
Component: ProjectExplorer
- selektiere Element "MyFirstProject" in Baum <ProjektBaum>
