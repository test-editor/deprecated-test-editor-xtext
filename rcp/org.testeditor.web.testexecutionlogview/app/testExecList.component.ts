import { Component, Input } from '@angular/core';
import {LogGroup, TestExecutionLog} from "./model";
import {AppComponent} from "./app.component"

@Component({
    moduleId: module.id,
    selector: 'testExecList',
    templateUrl: 'testExecList.components.html'
})
export class TestExecutionListComponent {
    @Input() testruns:TestExecutionLog[] = [];
    @Input() app: AppComponent;

    onSelect(testExecLog: TestExecutionLog) {
        this.app.onSelect(testExecLog);
    }
}