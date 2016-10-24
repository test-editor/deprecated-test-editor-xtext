import { Component } from '@angular/core';
import { TestLogService } from './testLog.service';

@Component({
    selector: 'testexeclog-app',
    templateUrl: 'app/testexeclogapp.html'
})
export class AppComponent {
    testRuns: String[];

    constructor(private logService: TestLogService) {
        console.log(logService)
        this.logService.getTestExecutionLogs().then(testExecLogs => this.testRuns=testExecLogs)
    }

 }
