import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { HttpModule } from "@angular/http";
import { AppComponent }  from './app.component';
import {NgbModule} from '@ng-bootstrap/ng-bootstrap';
import {TestLogService} from "./testLog.service";
import {TestExecutionListComponent} from './testExecList.component';

@NgModule({
  imports: [ NgbModule.forRoot(), BrowserModule, HttpModule ],
  declarations: [ AppComponent,
                TestExecutionListComponent ],
  providers:    [ TestLogService],
  bootstrap: [ AppComponent ]
})
export class AppModule { }
