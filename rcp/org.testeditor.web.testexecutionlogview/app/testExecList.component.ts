import { Component, Input } from '@angular/core';
import {LogGroup, TestExecutionLog} from "./model";

@Component({
    moduleId: module.id,
    selector: 'testExecList',
    templateUrl: 'testExecList.components.html'
})
export class TestExecutionListComponent {
    @Input() testruns:TestExecutionLog[] = [];
}