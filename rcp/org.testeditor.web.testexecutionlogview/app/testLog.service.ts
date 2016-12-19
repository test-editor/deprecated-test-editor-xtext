import { Injectable } from '@angular/core';
import { Http, Headers } from '@angular/http';
import 'rxjs/add/operator/toPromise';
import { LogGroup } from "./model";
import { TestExecutionLog } from "./model";

@Injectable()
export class TestLogService {

    port: String

    constructor(private http: Http) {
        this.port = window.location.port
        // Use this for local setup with nodejs for development cycle
        // this.port = "19091"
    }

    getTestExecutionLogs(): Promise<TestExecutionLog[]> {
        return this.http.get('http://localhost:' + this.port + '/services/testruns').toPromise().then(response =>
            response.json().entries as TestExecutionLog[]
        ).catch(this.handleError);
    }

    getTestExecutionLogWithContent(filename: String): Promise<TestExecutionLog> {
        return this.http.get('http://localhost:' + this.port + '/services/testruns/' + filename + "/logGroups").toPromise().then(response =>
            response.json() as TestExecutionLog
        ).catch(this.handleError);
    }

    deleteTestRun(filename: String) {
        console.log('http://localhost:' + this.port + '/services/testruns/' + filename)
        this.http.delete('http://localhost:' + this.port + '/services/testruns/' + filename).toPromise().catch(this.handleError)
    }

    private handleError(error: any): Promise<any> {
        console.error('An error occurred', error); // for development purposes only
        return Promise.reject(error.message || error);
    }

}