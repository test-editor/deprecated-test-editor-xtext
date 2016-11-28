export class LogGroup {
    type: String
    logLines: String[]
    children: LogGroup[]
    name: String
}

export class TestExecutionLog {
    testStatistic: TestRunStatistic
    logGroups: LogGroup[]
    name: String
    testRunTimestamp: String
}

export class TestRunStatistic {
    tests: number 
    errors: number
    failures: number
}