import { Component } from '@angular/core';
import { TestLogService } from './testLog.service';
import {TestExecutionLog, LogGroup, TestRunStatistic} from './model';

@Component({
    selector: 'testexeclog-app',
    templateUrl: 'app/testexeclogapp.html'
})
export class AppComponent {
    testRuns: TestExecutionLog[]
    testRunLog: String
    logGroups: LogGroup[]
    testExecutionGroup: TestExecutionLog
    useLogGroup: boolean
    currentSelection: TestExecutionLog
    testStatistic:TestRunStatistic

    constructor(private logService: TestLogService) {
        this.logService.getTestExecutionLogs().then(testExecLogs => this.testRuns=testExecLogs)
        this.testRunLog = ''
        this.useLogGroup = true
    }

    onSelect(testExecLog: TestExecutionLog) {
        this.currentSelection = testExecLog
        if(this.useLogGroup) {
            this.logService.getTestExecutionLogGroups(testExecLog).then(logGroups => this.testExecutionGroup=logGroups)
            this.testStatistic = this.testExecutionGroup.testStatistic
            this.logGroups = this.testExecutionGroup.logGroups
        }else{
            this.logService.getTestExecutionLogContent(testExecLog).then(content => this.testRunLog=content)
            this.testStatistic = null
            this.logGroups = null
        }
    }

    onToggle() {
        this.useLogGroup = !this.useLogGroup
        this.onSelect(this.currentSelection)
    }
 }
