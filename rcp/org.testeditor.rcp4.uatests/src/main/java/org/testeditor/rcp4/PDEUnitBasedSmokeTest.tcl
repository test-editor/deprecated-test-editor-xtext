package org.testeditor.rcp4

import org.testeditor.rcp4.*

# PDEUnitBasedSmokeTest implements SmokeTest

* Validate initial views
Component: TestEditor
- Clean Workspace
- Restart Test-Editor with clean environment

Component: HauptFenster
 - ist View <ProjektBaum> sichtbar
 - ist View <TestStepSelector> sichtbar
  
Component: TestEditor
- Einfaches Projekt erzeugen

Component: ProjectExplorer
- selektiere Element "DemoProject" in Baum <ProjektBaum>

