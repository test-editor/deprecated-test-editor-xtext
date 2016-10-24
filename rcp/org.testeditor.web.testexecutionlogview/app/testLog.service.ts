import { Injectable } from '@angular/core';
import { Http,  Headers } from '@angular/http';
import 'rxjs/add/operator/toPromise';
import {LogGroup} from "./model";
import {TestExecutionLog} from "./model";

@Injectable()
export class TestLogService {

    constructor(private http: Http) {}

    getTestExecutionLogs(): Promise<String[]> {
        return this.http.get('http://localhost:19090/services/testexeclog/list').toPromise().then(response =>
            response.json().entries as TestExecutionLog[]
        ).catch(this.handleError);
    }

//    getLogTitle(): Promise<String> {
//        console.log("Title Service call");
//        return this.http.get('http://localhost:9090/services/testlog/title').toPromise().then(response =>
//            response.json().value as String
//        ).catch(this.handleError);
//    }

    getLogContent(): Promise<LogGroup[]> {
        console.log("Content Service call");
        return this.http.get('http://localhost:9090/services/testlog/content').toPromise().then(response =>
            response.json().logGroups as LogGroup[]
        ).catch(this.handleError);
    }

    private handleError(error: any): Promise<any> {
        console.error('An error occurred', error); // for demo purposes only
        return Promise.reject(error.message || error);
    }
}