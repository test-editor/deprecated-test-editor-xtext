import { Component, Input } from '@angular/core';
import {LogGroup} from "./model";


@Component({
    moduleId: module.id,
    selector: 'te-log-groups-view',
    templateUrl: 'LogGroupView.components.html'
})
export class LogGroupView {
    @Input() testruns:LogGroup[] = [];

}