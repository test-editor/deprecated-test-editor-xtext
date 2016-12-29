import { Injectable, EventEmitter } from '@angular/core';
import { Http, Headers } from '@angular/http';
import 'rxjs/add/operator/toPromise';
import { LogGroup } from "./model";
import { TestExecutionLog } from "./model";

@Injectable()
export class TestLogService {

    port: String
    knownTestruns: String[]
    public testExecutionLogsChanged = new EventEmitter<Object>()

    constructor(private http: Http) {
        this.port = window.location.port
        // Use this for local setup with nodejs for development cycle
        //this.port = "19091"
    }

    getTestExecutionLogs(): Promise<TestExecutionLog[]> {
        let result = this.http.get('http://localhost:' + this.port + '/services/testruns').toPromise().then(response =>
            response.json().entries as TestExecutionLog[]
        ).catch(this.handleError)
        result.then(testExecLogs => {
            this.knownTestruns = []
            for (let run of testExecLogs) {
                this.knownTestruns.unshift(run.name)
            }
        })
        return result
    }

    getTestExecutionLogWithContent(filename: String): Promise<TestExecutionLog> {
        let result = this.http.get('http://localhost:' + this.port + '/services/testruns/' + filename + "/logGroups").toPromise().then(response =>
            response.json() as TestExecutionLog
        ).catch(this.handleError);
        result.then(log => {
            if (!(this.knownTestruns.indexOf(log.name) > -1)) {
                this.knownTestruns.push(log.name)
                this.testExecutionLogsChanged.emit(log)
            }
        })
        return result
    }

    deleteTestRun(filename: String) {
        this.http.delete('http://localhost:' + this.port + '/services/testruns/' + filename).toPromise().catch(this.handleError)
    }

    getScreenshotURL(testRun: String, path: String): String {
        return 'http://localhost:' + this.port + '/services/testruns/' + testRun + "/" + path
    }

    private handleError(error: any): Promise<any> {
        console.error('An error occurred', error); // for development purposes only
        return Promise.reject(error.message || error)
    }

}