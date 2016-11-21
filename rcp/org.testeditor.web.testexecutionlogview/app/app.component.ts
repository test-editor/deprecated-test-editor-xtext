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
            this.logService.getTestExecutionLogGroups(testExecLog).then(testRunLog => {
                this.logGroups = testRunLog.logGroups
                this.testStatistic = testRunLog.testStatistic 
            })
        }else{
            this.logService.getTestExecutionLogContent(testExecLog).then(content => this.testRunLog=content)
        }
    }

    onToggle() {
        this.useLogGroup = !this.useLogGroup
        this.onSelect(this.currentSelection)
    }
 }
