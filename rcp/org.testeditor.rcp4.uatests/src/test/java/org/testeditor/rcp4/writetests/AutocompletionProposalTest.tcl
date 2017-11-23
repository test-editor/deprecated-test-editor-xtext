import org.testeditor.rcp4.*
import org.testeditor.fixture.swt.*

# AutocompletionProposalTest

config TestEditorConfig

// the appropriate aml is added within the setup of this test

* Check that "myApp" ^is proposed ^as component

	Component: ActiveEditor
	// given
	- Kill content completely // remove any default generated file content	
	- Type "# App\n\n* do something\n\nComponent: " into active editor
	// when
	- hasMyApp = Has autocompletion proposal "myApp" when entering ""
	// then
	- assert hasMyApp

* Given an empty component reference _ content completion will already provide useful proposals _ "with wrong" ^is filtered since no element to satisfy the parameter ^is available

	Component: ActiveEditor
	// given
	// when
	- Use auto completion with "" and select "myApp"
	- Type "\n" into active editor
	// then
	- hasDoSomethingInteraction = Has autocompletion proposal "do something \"count: long\" - test step" when entering ""
	- hasDoDifferentlyInteraction = Has autocompletion proposal "do differently \"count: long\" - test step" when entering ""
	- shouldHaveDoWithRight = Has autocompletion proposal "with right <element> - test step" when entering ""
	- shouldNotHaveDoWithWrong = Has autocompletion proposal "with wrong <element> - test step" when entering ""
	- assert hasDoSomethingInteraction
	- assert hasDoDifferentlyInteraction
	- assert shouldHaveDoWithRight
	- assert !shouldNotHaveDoWithWrong

* Given a prefix of "- " _ selection of content completion works and text inserted ^does ^not add superfluous dash

	Component: ActiveEditor
	// given
	- Type "- " into active editor
	// when
	- Use auto completion with "" and select "do something \"count: long\" - test step"
	- Type "\n" into active editor
	// then
	- singleDashedInteraction = Contains editor "\n- do something \"1\""
	- assert singleDashedInteraction

* Given a prefixed of "- do some" _ assist provides only the ones limited to the ones matching that prefix

	Component: ActiveEditor
	// given
	- Type "\n- do some" into active editor
	// when
	- hasDoSome = Has autocompletion proposal "do something \"count: long\" - test step" when entering ""
	- hasDoSomeDiff = Has autocompletion proposal "do something different \"count: long\" - test step" when entering ""
	- shouldNotHaveDoDiff = Has autocompletion proposal "do differently \"count: long\" - test step" when entering ""
	// then
	- assert hasDoSome
	- assert hasDoSomeDiff
	- assert !shouldNotHaveDoDiff

* Given a content completion based on a prefix _ selecting the proposal inserts exactly the proposal and no superfluous dashes remain

	Component: ActiveEditor
	// given
	// when
	- Use auto completion with "" and select "do something different \"count: long\" - test step"
	- Type "\n" into active editor
	// then
	- singleDashedInteractionPrefixed = Contains editor "\n- do something different \"1\""
	- assert singleDashedInteractionPrefixed

* Given a prefix ending in a space eg "- do something different " _ proposal will still work and be narrowed apropriately

	Component: ActiveEditor
	// given
	- Type "\n- do something different " into active editor
	// when
	- hasDoSomeDiffEvenWithPrefixedSpace = Has autocompletion proposal "do something different \"count: long\" - test step" when entering ""
	- shoudlNotHaveDoSomeAnymore = Has autocompletion proposal "do something \"count: long\" - test step" when entering ""
	// then
	- assert hasDoSomeDiffEvenWithPrefixedSpace
	- assert !shoudlNotHaveDoSomeAnymore

* Given an assignment _ make sure only templates that actually ^return something are available

	Component: ActiveEditor
	// given
	- Type "\n\n- somevar = " into active editor
	// when
	- hasDoReturn = Has autocompletion proposal "doreturn something <element> - test step" when entering ""
	- shouldNotHaveSomething = Has autocompletion proposal "do something \"count: long\" - test step" when entering ""
	// then
	- assert hasDoReturn
	- assert !shouldNotHaveSomething
	
Setup:

	// test editor project
	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjectTree>

	// with gradle and web fixture
	Component: NewProjectDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>
	- Type "MyFirstWebProject" into <ProjectName>
	- Click on <NextButton>
	- Select value "Gradle" in combo box <BuildTool>
	- Select element "Web Fixture" in list <AvailableFixturesList>
	- Click on <AddFixtureButton>
	- Click on <FinishButton>
	- Wait for dialog "Progress Information" to popup and then close after at most "3" respectively "120" seconds

	Component: Workbench
	- Wait until all jobs finished

	// and a working aml
	Component: ProjectExplorer
	- Select element "MyFirstWebProject/src/test/java" in tree <ProjectTree>
	- Execute menu item "New/Application Mapping" in tree <ProjectTree>

	Component: NewTestCaseDialog
	- Type "App.aml" into <TestCaseName>
	- Click on <FinishButton>

	Component: ActiveEditor
	- Fill editor with text from "src/test/resources/AppAml.txt"
	- Save editor content

	// and an empty tcl
	Component: ProjectExplorer
	- Select element "MyFirstWebProject/src/test/java" in tree <ProjectTree>
	- Execute menu item "New/Test Case" in tree <ProjectTree>

	Component: NewTestCaseDialog
	- Type "App.tcl" into <TestCaseName>
	- Click on <FinishButton>
