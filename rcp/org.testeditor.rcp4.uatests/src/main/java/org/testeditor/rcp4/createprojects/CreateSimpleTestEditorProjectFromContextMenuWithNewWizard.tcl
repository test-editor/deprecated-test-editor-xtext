package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*;

#CreateSimpleTestEditorProjectFromContextMenuWithNewWizard implements CreateSimpleTestEditorProject

* start creation process
Component: ProjectExplorer
- execute menu item  "New/Project..."  in tree <ProjektBaum>
- Protkolliere vorhandene UI Elemente


* Select type Test Editor Project
Component: NewDialog

* Give the Project the name "MyFirstProject"



* Verify that the project is visible in the ui

