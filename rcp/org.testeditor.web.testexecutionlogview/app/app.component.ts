import { Component } from '@angular/core';
import { TestLogService } from './testLog.service';
import {TestExecutionLog} from './model';

@Component({
    selector: 'testexeclog-app',
    templateUrl: 'app/testexeclogapp.html'
})
export class AppComponent {
    testRuns: TestExecutionLog[];
    testRunLog: String;

    constructor(private logService: TestLogService) {
        console.log(logService)
        this.logService.getTestExecutionLogs().then(testExecLogs => this.testRuns=testExecLogs)
        this.testRunLog = '';
    }

    onSelect(testExecLog: TestExecutionLog) {
        this.logService.getTestExecutionLogContent(testExecLog).then(content => this.testRunLog=content)
    }
 }
