import { Component, OnInit } from '@angular/core';
import { TestExecutionLog } from "./model";
import { TestLogService } from './testLog.service';

@Component({
    moduleId: module.id,
    selector: 'testExecList',
    templateUrl: 'testExecList.components.html'
})
export class TestExecutionListComponent implements OnInit {

    testruns: TestExecutionLog[]

    constructor(private logService: TestLogService) {
    }

    deleteTestRun(testLog: TestExecutionLog) {
        this.logService.deleteTestRun(testLog.testRunTimestamp)
        this.testruns.splice(this.testruns.indexOf(testLog,0),1)
    }

    ngOnInit(): void {
        this.logService.getTestExecutionLogs().then(testExecLogs => this.testruns = testExecLogs)
    }

}