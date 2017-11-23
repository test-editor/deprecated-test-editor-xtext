// deactivated because workspace cleanup does not work (yet)
package org.testeditor.rcp4

import org.testeditor.fixture.swt.*

# CreateProjectOverServiceTest implements SmokeTest

config TestEditorConfig

* Validate initial views

	Component: TestEditorServices
	- Create simple project

	Component: ProjectExplorer
	- Select element "DemoProject" in tree <ProjectTree>
