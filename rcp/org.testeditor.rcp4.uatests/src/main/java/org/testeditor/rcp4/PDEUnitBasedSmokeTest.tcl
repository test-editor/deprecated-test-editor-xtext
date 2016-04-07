package org.testeditor.rcp4

import org.testeditor.rcp4.*

# PDEUnitBasedSmokeTest implements SmokeTest

* Validate initial views
Component: TestEditor
- Clean Workspace
- Reset UI State

Component: HauptFenster
 - ist View <ProjektBaum> visiable
 - ist View <TestStepSelector> visiable
  
Component: TestEditor
- Create simple project

Component: ProjectExplorer
- Select element "DemoProject" in tree <ProjektBaum>

