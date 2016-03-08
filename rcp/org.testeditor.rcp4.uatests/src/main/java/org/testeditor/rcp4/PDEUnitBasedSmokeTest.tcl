package org.testeditor.rcp4

import org.testeditor.rcp4.*

# PDEUnitBasedSmokeTest implements SmokeTest

* Validate initial views

Component: HauptFenster
 - ist View <ProjektBaum> sichtbar
 - ist View <TestStepSelector> sichtbar
 
 Component: TestEditor
- Einfaches Projekt erzeugen

Component: HauptFenster
- Warte "10" Sekunden