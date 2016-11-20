export class LogGroup {
    type: String
    logLines: String[]
    children: LogGroup[]
    name: String
}

export class TestExecutionLog {

    links: Link[]
    testStatistic: TestRunStatistic
    name: String
    filename: String
    
}

export class Link {
    rel: String
    href: String
}

export class TestRunStatistic {
    tests: number 
    errors: number
    failures: number
}