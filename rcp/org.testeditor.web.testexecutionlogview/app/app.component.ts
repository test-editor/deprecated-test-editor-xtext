import { Component } from '@angular/core';
import { TestLogService } from './testLog.service';
import {TestExecutionLog, LogGroup} from './model';

@Component({
    selector: 'testexeclog-app',
    templateUrl: 'app/testexeclogapp.html'
})
export class AppComponent {
    testRuns: TestExecutionLog[];
    testRunLog: String;
    logGroups: LogGroup[];
    useLogGroup: boolean;
    currentSelection: TestExecutionLog;

    constructor(private logService: TestLogService) {
        console.log(logService)
        this.logService.getTestExecutionLogs().then(testExecLogs => this.testRuns=testExecLogs)
        this.testRunLog = '';
        this.useLogGroup = false
    }

    onSelect(testExecLog: TestExecutionLog) {
        this.currentSelection = testExecLog
        if(this.useLogGroup) {
            this.logService.getTestExecutionLogGroups(testExecLog).then(logGroups => this.logGroups=logGroups)
        }else{
            this.logService.getTestExecutionLogContent(testExecLog).then(content => this.testRunLog=content)
        }
    }

    onToggle() {
        this.useLogGroup = !this.useLogGroup
        this.onSelect(this.currentSelection)
    }
 }
