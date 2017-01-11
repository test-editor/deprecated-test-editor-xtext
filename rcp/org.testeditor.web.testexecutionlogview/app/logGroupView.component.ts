import { Component, Input } from '@angular/core';
import { LogGroup } from "./model";
import { TestLogService } from "./testLog.service";

@Component({
    moduleId: module.id,
    selector: 'te-log-groups-view',
    templateUrl: 'LogGroupView.components.html'
})
export class LogGroupView {
    @Input() testruns: LogGroup[] = [];
    @Input() testRun: String = "";

    constructor(private logService: TestLogService) {
    }

}