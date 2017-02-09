import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
import { TestLogService } from './testLog.service';
import { LogGroup, TestRunStatistic } from './model';
import 'rxjs/add/operator/switchMap';

@Component({
    moduleId: module.id,
    selector: 'te-run-view',
    templateUrl: 'testRunView.component.html'
})
export class TestRunView implements OnInit {

    logGroups: LogGroup[]
    testStatistic: TestRunStatistic
    resultTableStyle: String
    runName: String
    testRunName: String

    constructor(private route: ActivatedRoute, private logService: TestLogService) {
    }

    ngOnInit(): void {
        this.route.params.forEach((params: Params) => {
            if (params['id'] !== undefined) {
                this.logService.getTestExecutionLogWithContent(params['id']).then(testRunLog => {
                    this.logGroups = testRunLog.logGroups
                    this.testStatistic = testRunLog.testStatistic
                    this.runName = testRunLog.name
                    this.testRunName = testRunLog.testRunTimestamp
                    if (this.testStatistic.errors == 0 && this.testStatistic.failures == 0) {
                        this.resultTableStyle = 'table table-success'
                    }
                    else {
                        this.resultTableStyle = 'table table-danger'
                    }
                    if (this.testStatistic.tests == 0) {
                        this.resultTableStyle = 'table table-warning'
                    }
                })
            }
        })
    }

}