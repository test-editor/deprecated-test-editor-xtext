import { Component, Input, OnInit } from '@angular/core';
import { TestLogService } from './testLog.service';
import { TestExecutionLog, LogGroup, TestRunStatistic } from './model';

@Component({
    moduleId: module.id,
    selector: 'te-run-view',
    templateUrl: 'TestRunView.component.html'
})
export class TestRunView implements OnInit {

    logGroups: LogGroup[]
    testStatistic: TestRunStatistic
    lgoService: TestLogService

    constructor(private logService: TestLogService) {
        this.logService = logService
    }

    //    @Input() currentSelection: TestExecutionLog;

    ngOnInit(): void {
        this.logService.getTestExecutionLogContent().then(testRunLog => {
        })
    }
}