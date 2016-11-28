import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { HttpModule } from "@angular/http";
import { RouterModule } from '@angular/router';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { AppComponent } from './app.component';
import { TestLogService } from "./testLog.service";
import { TestExecutionListComponent } from './testExecList.component';
import { LogGroupView } from './logGroupView.component';
import { TestRunView } from './testRunView.component';


@NgModule({
  imports: [
    NgbModule.forRoot(),
    BrowserModule,
    HttpModule,
    RouterModule.forRoot([
      {
        path: 'testrun/:id',
        component: TestRunView
      }
      ,
      {
        path: '',
        component: TestRunView
      }
    ],{ useHash: true })
  ],
  declarations: [
    AppComponent,
    TestExecutionListComponent,
    LogGroupView,
    TestRunView],
  providers: [TestLogService],
  bootstrap: [AppComponent]
})
export class AppModule { }
