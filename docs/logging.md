# Logging concept

## requirements

*all logging output of all components that are part of the test run must be available within one log file (as far as technically possible)*

-   e.g. logging of the different fixtures are written to this (single) file, too
-   logging output of triggered applications that do not output to stdout or cannot be configured via log4j etc. may be of a problem here
-   fixture need to make up for that, that is, a fixture is responsible to collect all logs and files that the test run cannot take care
    of and add it to the test run

*logs are persisted along with the testrun (result), including files generated during the test run that are deemed important*

-   on ci these artifacts are archived and thus accessible
-   fixtures are responsible to take care of files and logs not accessible to the test run

*logs can be viewed during test execution (within the rcp) as well after the test was run (even if run on another machine)*

-   given an open rcp, the log (activity) must be visible during test execution as well as after the test run
-   the rcp allows selection of logs previously executed (local history only)
-   given an archived test execution log (and all dependent files), these files can be viewed (need not be within the rcp)
-   given an archived test execution log (and all dependent files), a tester / developer can start analytical sessions to pin point
    problems
-   whether the above should be realised via rcp is an open issue

*logging can be configured to selectively have different log level on different "components"*

-   log4j provides possibilities to do that

## output in rcp

-   idea: web view, populated during run
-   executing a test will pipe the output into a log file (and into the console)
-   the console itself is meant for troubleshooting
-   the new log console view (wich is an angularJS app) is meant for the tester
-   the log file is (partially) parsed (by the rcp) and displayed in an angularJS application that gets the
    log-information pushed via a rest-service
-   idea: log is not parsed! the enter/leave calls from testexecution are put into a defined data structure (e.g. json with a schema) that
    is written separately. This separate file has references to the log such that each entry can be associated with the respective region of
    the log file => no log file parsing necessary?
-   this web app is then displayed in a log window within the rcp itself
-   future: rcp will be able to select an arbitrary log file of a test run and display this via the above mechanism
    within the rcp
-   future: selecting log entries should bring you to the respective code line within the test

## rcp select logs for run

-   future: logs need to be selectable via junit run
-   future: there should be a way for logs of tests that were run (and archived) on the ci, to be imported/selected in
    the rcp, thus allowing to view the logs in a structured way

## where are logs generated

-   logging is done using loggers provided by slf4j
-   tests may inherit AbstractTestCase which initializes a DefaultTestRunReporter (accessible via instance variable "reporter")
```
public class GreetingTest extends AbstractTestCase {
  @Test
  public void execute() throws Exception {
    reporter.enter(TestRunReporter.SemanticUnit.SPECIFICATION_STEP, "Start the famous greetings application");
    // ...
  }
}
```
-   every generated test makes calls to this reporter to indicate activity that should be reported
-   the TestRunReporter (implemented by DefaultTestRunReporter) allows Listeners to register for activities
```
public class SomeFixture implements TestRunListener, TestRunReportable {

    // is automatically called by the constructor of the class which declares the class local
    // variable of this fixture (either the test class itself, or the setup config class that is generated)
	public void initWithReporter(TestRunReporter reporter) {
		reporter.addListener(this);
	}

    
    // is called whenever the reporter is called from the generated test calss. 
    // the method must ignore messages it is not interested in
	@Override
	public void reported(SemanticUnit unit, Action action, String msg) {
      // do something
	}
    
    // ...
    
}
```
-   the DefaultTestRunReporter automatically reports every activity to the DefaultLoggingListener
-   the DefaultLoggingListener logs entering and leaving Tests, Specification Steps, Components and Test steps (currently)
-   future: this will be extended to report Macro-Calls, too
-   log4j2 configuration is expected to specify where logging is to be written to

## where are logs archived

-   rcp does no archiving
-   archiving is done by jenkins
-   future: jenkins should specify log-configuration files (log4j2.xml) to make sure that logs are put where jenkins
    expects them, right?

## any changes in mvn / gradle build

-   since fixture itself is responsible to save/copy all artifacts necessary to the respective location, no changes in build need to be done
-   how is the configuration of the jenkins archiving connected to the log destination of the fixture? fixture should be able to get the
    folder jenkins is actually archiving

