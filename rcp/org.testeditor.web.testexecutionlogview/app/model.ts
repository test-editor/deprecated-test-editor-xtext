export class LogGroup {
    type: String
    logLines: String[]
    children: LogGroup[]
    name: String
}

export class TestExecutionLog {

    links: Link[]
    name: String
    filename: String
    
}

export class Link {
    rel: String
    href: String
}