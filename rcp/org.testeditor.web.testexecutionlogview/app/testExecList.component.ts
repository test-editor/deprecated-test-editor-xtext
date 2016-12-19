import { Component, Input } from '@angular/core';
import { LogGroup, TestExecutionLog } from "./model";
import { AppComponent } from "./app.component"
import { TestLogService } from './testLog.service';

@Component({
    moduleId: module.id,
    selector: 'testExecList',
    templateUrl: 'testExecList.components.html'
})
export class TestExecutionListComponent {

    constructor(private logService: TestLogService) {
    }

    @Input() testruns: TestExecutionLog[] = [];

    deleteTestRun(filename: String) {
        this.logService.deleteTestRun(filename)
        this.logService.getTestExecutionLogs().then(testRuns => this.testruns = testRuns)
    }

}