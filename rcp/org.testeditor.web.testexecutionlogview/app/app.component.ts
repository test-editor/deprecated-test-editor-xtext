import { Component, OnInit } from '@angular/core';
import { TestLogService } from './testLog.service';
import { TestExecutionLog, LogGroup, TestRunStatistic } from './model';

@Component({
    selector: 'testexeclog-app',
    templateUrl: 'app/testexeclogapp.html'
})
export class AppComponent implements OnInit {
    testRuns: TestExecutionLog[]

    constructor(private logService: TestLogService) {
    }

    ngOnInit(): void {
        this.logService.getTestExecutionLogs().then(testExecLogs => this.testRuns = testExecLogs)
    }

}
